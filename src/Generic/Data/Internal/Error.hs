{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Error messages.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

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

-- |
-- >>> :set -XDeriveGeneric -XDerivingVia
-- >>> import Generic.Data (Generically(..))
-- >>> :{
--   data AB = A | B
--     deriving stock Generic
--     deriving Semigroup via Generically AB
-- :}
-- ...
--     • Cannot derive Semigroup instance for AB due to sum type
--     • When deriving the instance for (Semigroup AB)
type AssertNoSum (constraint :: Type -> Constraint) a =
    Assert (Not (HasSum (Rep a)))
    ('Text "Cannot derive " ':<>: 'ShowType constraint ':<>:
        'Text " instance for " ':<>: 'ShowType a ':<>: 'Text " due to sum type")
