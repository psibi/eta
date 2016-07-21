module GHCVM.DsForeign where

import TcRnMonad
import TypeRep
import CoreSyn
import DsCCall
import DsMonad
import HsSyn
import DataCon
import CoreUnfold
import Id
import Literal
import Module
import Name
import Type
import TyCon
import Coercion
import TcEnv
import TcType

import HscTypes
import ForeignCall
import TysWiredIn
import TysPrim
import PrelNames
import BasicTypes
import SrcLoc
import Outputable
import FastString
import DynFlags
import Platform
import Config
import OrdList
import Pair
import Util
import Hooks

import Data.Maybe
import Data.List

type Binding = (Id, CoreExpr)

dsForeigns :: [LForeignDecl Id] -> DsM (ForeignStubs, OrdList Binding)
dsForeigns p = return (NoStubs, nilOL)
dsForeigns fdecls = do
  fives <- mapM doLDecl fdecls
  -- TODO: Finish
  return (undefined, undefined)
  where doLDecl (L loc decl) = putSrcSpanDs loc (doDecl decl)
        doDecl (ForeignImport id _ co spec) = do
          (bs, h, c) <- dsFImport (unLoc id) co spec
          return (h, c, [], bs)
        doDecl fi = pprPanic "doDecl: Not implemented" (ppr fi)

dsFImport :: Id -> Coercion -> ForeignImport -> DsM ([Binding], SDoc, SDoc)
dsFImport id co (CImport cconv safety mHeader spec _) = do
  (ids, h, c) <- dsCImport id co spec (unLoc cconv) (unLoc safety) mHeader
  return (ids, h, c)

dsCImport :: Id -> Coercion -> CImportSpec -> CCallConv -> Safety
  -> Maybe Header -> DsM ([Binding], SDoc, SDoc)
dsCImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsCImport id co (CFunction target) cconv safety mHeader
  = dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsCImport id _ _ _ _ _ = pprPanic "doCImport: Not implemented" (ppr id)

dsPrimCall :: Id -> Coercion -> ForeignCall -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsPrimCall funId co fcall = do
  args <- mapM newSysLocalDs argTypes
  ccallUniq <- newUnique
  dflags <- getDynFlags
  let callApp = mkFCall dflags ccallUniq fcall (map Var args) ioResType
      rhs = mkLams tvs (mkLams args callApp)
      rhs' = Cast rhs co
  return ([(funId, rhs')], empty, empty)
  where ty                    = pFst $ coercionKind co
        (tvs, funTy)          = tcSplitForAllTys ty
        (argTypes, ioResType) = tcSplitFunTys funTy

dsFCall :: Id -> Coercion -> ForeignCall -> Maybe Header
  -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsFCall funId co fcall mDeclHeader = do
  args <- mapM newSysLocalDs argTypes
  (valArgs, argWrappers) <- mapAndUnzipM unboxArg (map Var args)
  let workArgIds = [v | Var v <- valArgs]
  (ccallResultType, resWrapper) <- boxResult ioResType
  ccallUniq <- newUnique
  workUniq <- newUnique
  dflags <- getDynFlags
  -- Build worker
  let workerType = mkForAllTys tvs $
                   mkFunTys (map idType workArgIds) ccallResultType
      ccallApp   = mkFCall dflags ccallUniq fcall valArgs ccallResultType
      workRhs    = mkLams tvs (mkLams workArgIds ccallApp)
      workId     = mkSysLocal (fsLit "$wccall") workUniq workerType

  -- Build wrapper
  let workApp        = mkApps (mkVarApps (Var workId) tvs) valArgs
      wrapperBody    = foldr ($) (resWrapper workApp) argWrappers
      wrapRhs        = mkLams (tvs ++ args) wrapperBody
      wrapRhs'       = Cast wrapRhs co
      funIdWithInline = funId
                       `setIdUnfolding`
                       mkInlineUnfolding (Just (length args)) wrapRhs'
  return ([(workId, workRhs), (funIdWithInline, wrapRhs')], empty, empty)

  where ty = pFst $ coercionKind co
        (tvs, funTy) = tcSplitForAllTys ty
        (argTypes, ioResType) = tcSplitFunTys funTy