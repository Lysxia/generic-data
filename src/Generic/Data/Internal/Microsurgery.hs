{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    PolyKinds,
    TypeFamilies,
    TypeOperators #-}

module Generic.Data.Internal.Microsurgery where

import Data.Coerce (Coercible, coerce)
import GHC.Generics

import Generic.Data.Types

unrecordify ::
  Coercible (Unrecordify f p) (f p) =>
  -- Coercible is not symmetric!??
  Data f p -> Data (Unrecordify f) p
unrecordify = coerce

recordify ::
  Coercible (f p) (Unrecordify f p) =>
  Data (Unrecordify f) p -> Data f p
recordify = coerce

-- | Forget that a type was declared using record syntax.
--
-- @
-- data Foo = Bar { baz :: Zap }
--
-- -- becomes --
--
-- data Foo = Bar Zap
-- @
--
-- Concretely, set the last field of 'MetaCons' to 'False' and forget field
-- names.
type family Unrecordify (f :: k -> *) :: k -> *
type instance Unrecordify (M1 D m f) = M1 D m (Unrecordify f)
type instance Unrecordify (f :+: g) = Unrecordify f :+: Unrecordify g
type instance Unrecordify (f :*: g) = Unrecordify f :*: Unrecordify g
type instance Unrecordify (M1 C ('MetaCons nm fx _isRecord) f) = M1 C ('MetaCons nm fx 'False) (Unrecordify f)
type instance Unrecordify (M1 S ('MetaSel _nm su ss ds) f) = M1 S ('MetaSel 'Nothing su ss ds) f
type instance Unrecordify V1 = V1
type instance Unrecordify U1 = U1
