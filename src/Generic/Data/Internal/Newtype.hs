{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Pack/unpack newtypes.

module Generic.Data.Internal.Newtype where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint)
import GHC.Generics (Generic(..), D1, C1, S1, K1)
import GHC.TypeLits (TypeError, ErrorMessage(..))

import Generic.Data.Internal.Meta (MetaDataNewtype, MetaOf)

class (Generic a, Coercible a (Old a), Newtype' a) => Newtype a
instance (Generic a, Coercible a (Old a), Newtype' a) => Newtype a

type Old a = GOld (Rep a)

type family GOld (f :: k -> *) where
  GOld (D1 _d (C1 _c (S1 _s (K1 _i b)))) = b

type Newtype' a = NewtypeErr a (MetaDataNewtype (MetaOf (Rep a)))

type family NewtypeErr a (b :: Bool) :: Constraint where
  NewtypeErr a 'True = ()
  NewtypeErr a 'False = TypeError
    ('Text "The type " ':<>: 'ShowType a ':<>: 'Text " is not a newtype.")

unpack :: Newtype a => a -> Old a
unpack = coerce

pack :: Newtype a => Old a -> a
pack = coerce
