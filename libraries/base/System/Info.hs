{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Info
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Information about the characteristics of the host
-- system lucky enough to run your program.
--
-----------------------------------------------------------------------------

module System.Info
   (
       os,
       arch,
       bigEndian,
       compilerName,
       compilerVersion
   ) where

import Data.Version
import GHC.Pack (unpackCString)

-- | The version of 'compilerName' with which the program was compiled
-- or is being interpreted.
compilerVersion :: Version
compilerVersion = Version [major, minor] []
  where (major, minor) = compilerVersionRaw `divMod` 100

-- | The operating system on which the program is running.
-- | NOTE: This returns the system property os.name.
os :: String
os = unpackCString os'

-- | The machine architecture on which the program is running.
-- | NOTE: This returns the system property os.arch.
arch :: String
arch = unpackCString os'

-- | The Haskell implementation with which the program was compiled
-- or is being interpreted.
compilerName :: String
compilerName = "eta"

compilerVersionRaw :: Int
compilerVersionRaw = 0001

foreign import java unsafe "@static eta.base.Utils.getOS" os' :: JString
foreign import java unsafe "@static eta.base.Utils.getArch" arch' :: JString
foreign import java unsafe "@static eta.base.Utils.isBigEndian" bigEndian :: Bool
