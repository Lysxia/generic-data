-- | Type level functions on generic representations.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

nconstructors :: forall r. KnownNat (NConstructors r) => Integer
nconstructors = natVal @(NConstructors r) Proxy

-- | Arity of a constructor.
type family   NFields (r :: k -> Type) :: Nat
type instance NFields (M1 C c f) = NFields f
type instance NFields (f :*: g)  = NFields f + NFields g
type instance NFields (M1 S c f) = 1

nfields :: forall r. KnownNat (NFields r) => Integer
nfields = natVal @(NFields r) Proxy
