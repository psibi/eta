{-# LANGUAGE CPP, DeriveDataTypeable #-}

module ETA.HsSyn.HsDoc (
  HsDocString(..),
  LHsDocString,
  ppr_mbDoc
  ) where

#include "HsVersions.h"

import ETA.Utils.Outputable
import ETA.BasicTypes.SrcLoc
import ETA.Utils.FastString

import Data.Data

newtype HsDocString = HsDocString FastString
  deriving (Eq, Show, Data, Typeable)

type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr (HsDocString fs) = ftext fs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

