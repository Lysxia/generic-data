{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    PolyKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

module Generic.Data.Internal.Microsurgery where

import Data.Coerce (Coercible, coerce)
import GHC.Generics
import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError)

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

-- * Renaming

-- | @f \@\@ s@ is the application of a type-level function symbolized by @f@
-- to a @s :: 'Symbol'@.
type family (f :: *) @@ (s :: Symbol) :: Symbol

-- | Identity function @'Symbol' -> 'Symbol'@.
data SId
type instance SId @@ s = s

-- | Empty function (compile-time error when applied).
data SError
type instance SError @@ s = TypeError ('Text "Invalid name: " ':<>: 'ShowType s)

-- | Specify a function for a fixed set of strings, and fall back to @f@ for the others.
data SRename (xs :: [(Symbol, Symbol)]) (f :: *)
type instance SRename xs f @@ s = SRename' xs f s

-- | Closed type family for 'SRename'.
type family SRename' (xs :: [(Symbol, Symbol)]) (f :: *) (s :: Symbol) where
  SRename' '[] f s = f @@ s
  SRename' ('( s,  t) ': _xs) _f s = t
  SRename' ('(_r, _t) ':  xs)  f s = SRename' xs f s
