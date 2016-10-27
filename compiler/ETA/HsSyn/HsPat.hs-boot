{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
#if __GLASGOW_HASKELL__ > 706
{-# LANGUAGE RoleAnnotations #-}
#endif

module ETA.HsSyn.HsPat where
import ETA.BasicTypes.SrcLoc( Located )

import Data.Data hiding (Fixity)
import ETA.Utils.Outputable
import ETA.HsSyn.PlaceHolder      ( DataId )

#if __GLASGOW_HASKELL__ > 706
type role Pat nominal
#endif
data Pat (i :: *)
type LPat i = Located (Pat i)

#if __GLASGOW_HASKELL__ > 706
instance Typeable Pat
#else
instance Typeable1 Pat
#endif

instance (DataId id) => Data (Pat id)
instance (OutputableBndr name) => Outputable (Pat name)
