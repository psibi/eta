{-#LANGUAGE GeneralizedNewtypeDeriving#-}

module ETA.Interactive.Types where

import Data.Word
import Data.Bits
import ETA.Main.DynFlags
import ETA.Utils.Platform (platformWordSize)
import ETA.Utils.Panic (panic)

-- StgWord is a type representing an StgWord on the target platform.
-- A Word64 is large enough to hold a Word for either a 32bit or 64bit platform
newtype StgWord = StgWord Word64
    deriving (Eq, Bits)

fromStgWord :: StgWord -> Integer
fromStgWord (StgWord i) = toInteger i

toStgWord :: DynFlags -> Integer -> StgWord
toStgWord dflags i
    = case platformWordSize (targetPlatform dflags) of
      -- These conversions mean that things like toStgWord (-1)
      -- do the right thing
      4 -> StgWord (fromIntegral (fromInteger i :: Word32))
      8 -> StgWord (fromInteger i :: Word64)
      w -> panic ("toStgWord: Unknown platformWordSize: " ++ show w)
