{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsPat where
import SrcLoc( Located )

import Data.Data hiding (Fixity)
import Outputable

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)

instance (Data id) => Data (Pat id)
instance (OutputableBndr name) => Outputable (Pat name)
