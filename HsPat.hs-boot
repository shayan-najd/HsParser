{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsPat where
import SrcLoc( Located )

import Data.Data hiding (Fixity)

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)

instance (Data id) => Data (Pat id)
