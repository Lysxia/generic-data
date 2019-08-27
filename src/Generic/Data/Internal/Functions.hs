{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level functions on generic representations.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Functions where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

-- | Number of constructors of a data type.
type family   NConstructors (r :: k -> Type) :: Nat
type instance NConstructors (M1 D c f) = NConstructors f
type instance NConstructors (f :+: g)  = NConstructors f + NConstructors g
type instance NConstructors (M1 C c f) = 1

-- | Number of constructors of a data type.
nconstructors :: forall r. KnownNat (NConstructors r) => Integer
nconstructors = natVal @(NConstructors r) Proxy

-- | Arity of a constructor.
type family   NFields (r :: k -> Type) :: Nat
type instance NFields (M1 C c f) = NFields f
type instance NFields (f :*: g)  = NFields f + NFields g
type instance NFields (M1 S c f) = 1

-- | Arity of a constructor.
nfields :: forall r. KnownNat (NFields r) => Integer
nfields = natVal @(NFields r) Proxy
