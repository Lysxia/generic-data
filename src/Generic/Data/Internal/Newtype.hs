{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- | Class of newtypes. There is an instance @'Newtype' a@ if and only if @a@
-- is a newtype and an instance of 'Generic'.
class (Generic a, Coercible a (Old a), Newtype' a) => Newtype a
instance (Generic a, Coercible a (Old a), Newtype' a) => Newtype a

-- | The type wrapped by a newtype.
--
-- @
-- newtype Foo = Foo { bar :: Bar } deriving 'Generic'
-- -- Old Foo ~ Bar
-- @
type Old a = GOld (Rep a)

type family GOld (f :: * -> *) where
  GOld (D1 _d (C1 _c (S1 _s (K1 _i b)))) = b

-- | Use 'Newtype' instead.
type Newtype' a = NewtypeErr a (MetaDataNewtype (MetaOf (Rep a)))

type family NewtypeErr a (b :: Bool) :: Constraint where
  NewtypeErr a 'True = ()
  NewtypeErr a 'False = TypeError
    ('Text "The type " ':<>: 'ShowType a ':<>: 'Text " is not a newtype.")

-- | Generic newtype destructor.
unpack :: Newtype a => a -> Old a
unpack = coerce

-- | Generic newtype constructor.
pack :: Newtype a => Old a -> a
pack = coerce
