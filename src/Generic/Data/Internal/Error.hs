{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Data.Internal.Error where

import Data.Kind
import Data.Type.Bool
import GHC.Generics
import GHC.TypeLits

type family HasSum f where
  HasSum V1 = 'False
  HasSum U1 = 'False
  HasSum (K1 i c) = 'False
  HasSum (M1 i c f) = HasSum f
  HasSum (f :*: g) = HasSum f || HasSum g
  HasSum (f :+: g) = 'True

class Assert (pred :: Bool) (msg :: ErrorMessage)
instance Assert 'True msg
instance (TypeError msg ~ '()) => Assert 'False msg

type AssertNoSum (constraint :: * -> Constraint) a =
    Assert (Not (HasSum (Rep a)))
    ('Text "Cannot derive " ':<>: 'ShowType constraint ':<>:
        'Text " instance for " ':<>: 'ShowType a ':<>: 'Text " due to sum type")
