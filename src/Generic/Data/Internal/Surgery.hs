{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operate on data types: insert/modify/delete fields and constructors.

module Generic.Data.Internal.Surgery where

import Data.Kind (Constraint)
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeNats
import GHC.TypeLits

import Generic.Data.Internal.Compat (Div)
import Generic.Data.Internal.Defun

type family   Linearize (f :: k -> *) :: k -> *
type instance Linearize (M1 d m f) = M1 d m (LinearizeSum f V1)

type family   LinearizeSum (f :: k -> *) (tl :: k -> *) :: k -> *
type instance LinearizeSum V1 tl = tl
type instance LinearizeSum (f :+: g) tl = LinearizeSum f (LinearizeSum g tl)
type instance LinearizeSum (M1 c m f) tl = M1 c m (LinearizeProduct f U1) :+: tl

type family   LinearizeProduct (f :: k -> *) (tl :: k -> *) :: k -> *
type instance LinearizeProduct U1 tl = tl
type instance LinearizeProduct (f :*: g) tl = LinearizeProduct f (LinearizeProduct g tl)
type instance LinearizeProduct (M1 s m f) tl = M1 s m f :*: tl

class GLinearize f where
  gLinearize :: f x -> Linearize f x

instance GLinearizeSum f V1 => GLinearize (M1 d m f) where
  gLinearize (M1 a) = M1 (gLinearizeSum @_ @V1 (Left a))

class GLinearizeSum f tl where
  gLinearizeSum :: Either (f x) (tl x) -> LinearizeSum f tl x

instance GLinearizeSum V1 tl where
  gLinearizeSum (Left !_) = error "impossible"
  gLinearizeSum (Right c) = c

instance (GLinearizeSum g tl, GLinearizeSum f (LinearizeSum g tl))
  => GLinearizeSum (f :+: g) tl where
  gLinearizeSum (Left (L1 a)) = gLinearizeSum @_ @(LinearizeSum g tl) (Left a)
  gLinearizeSum (Left (R1 b)) = gLinearizeSum @f (Right (gLinearizeSum @g @tl (Left b)))
  gLinearizeSum (Right c) = gLinearizeSum @f (Right (gLinearizeSum @g (Right c)))

type family   Arborify (f :: k -> *) :: k -> *
type instance Arborify (M1 d m f) = M1 d m (ArborifySum f (CoArity f))

type family   ArborifySum (f :: k -> *) (n :: Nat) :: k -> *
type instance ArborifySum V1 n = V1
type instance ArborifySum (f :+: g) n =
  If (n == 1)
    (ArborifyProduct f (Arity f))
    (ArborifySum2 (Div n 2) (n - Div n 2) (SplitAt (Div n 2) (f :+: g)))

type family   SplitAt (n :: Nat) (f :: k -> *) :: (k -> *, k -> *)
type instance SplitAt n (f :+: g) =
  If (n == 0) '(V1, f :+: g) (ConsFst (:+:) f (SplitAt (n-1) g))
type instance SplitAt n (f :*: g) =
  If (n == 0) '(U1, f :*: g) (ConsFst (:*:) f (SplitAt (n-1) g))

type family   ArborifyProduct (f :: k -> *) (n :: nat) :: k -> *
type instance ArborifyProduct U1 n = U1
type instance ArborifyProduct (f :*: g) n =
  If (n == 1)
    f
    (ArborifyProduct2 (Div n 2) (n - Div n 2) (SplitAt (Div n 2) (f :+: g)))

type family   ConsFst (cons :: (k -> *) -> (k -> *) -> k -> *)
                (f :: k -> *) (e :: (k -> *, k -> *)) :: (k -> *, k -> *)
type instance ConsFst cons f '(g, h) = '(cons f g, h)

type family   ArborifySum2 (m :: Nat) (n :: Nat) (e :: (k -> *, k -> *)) :: k -> *
type instance ArborifySum2 m n '(f, g) = ArborifySum f m :+: ArborifySum g n

type family   ArborifyProduct2 (m :: Nat) (n :: Nat) (e :: (k -> *, k -> *)) :: k -> *
type instance ArborifyProduct2 m n '(f, g) = ArborifyProduct f m :*: ArborifyProduct g n

type family   FieldTypeAt (n :: Nat) (f :: k -> *) :: *
type instance FieldTypeAt n (M1 i c f) = FieldTypeAt n f
type instance FieldTypeAt n (f :*: g) = If (n == 0) (FieldTypeOf f) (FieldTypeAt (n-1) g)

type family   FieldTypeOf (f :: k -> *) :: *
type instance FieldTypeOf (M1 s m (K1 i a)) = a

type family   RemoveField (n :: Nat) (f :: k -> *) :: k -> *
type instance RemoveField n (M1 i m f) = M1 i m (RemoveField n f)
type instance RemoveField n (f :*: g) = If (n == 0) g (f :*: RemoveField (n-1) g)

-- | Position of a record field
type family   FieldIndex (field :: Symbol) (f :: k -> *) :: Nat
type instance FieldIndex field (M1 D m f) = FieldIndex field f
type instance FieldIndex field (M1 C m f) = FieldIndex field f
type instance FieldIndex field (M1 S ('MetaSel ('Just field') su ss ds) f :*: g)
  = If (field == field') 0 (1 + FieldIndex field g)

-- | Number of fields of a single constructor
type family   Arity (f :: k -> *) :: Nat
type instance Arity (M1 d m f) = Arity f
type instance Arity (f :*: g) = Arity f + Arity g
type instance Arity (K1 i c) = 1
type instance Arity U1 = 0

-- | Number of constructors of a data type
type family   CoArity (f :: k -> *) :: Nat
type instance CoArity (M1 D m f) = CoArity f
type instance CoArity (M1 C m f) = 1
type instance CoArity V1         = 0
type instance CoArity (f :+: g)  = CoArity f + CoArity g

class GRemoveField (n :: Nat) f where
  gRemoveField :: f x -> (FieldTypeAt n f, RemoveField n f x)

instance GRemoveField n f => GRemoveField n (M1 i c f) where
  gRemoveField (M1 a) = M1 <$> gRemoveField @n a

instance (If (n == 0) (() :: Constraint) (GRemoveField (n-1) g), IsBool (n == 0))
  => GRemoveField n (M1 s m (K1 i t) :*: g) where
  gRemoveField (a@(M1 (K1 t)) :*: b) = _If @(n == 0)
    (t, b)
    ((a :*:) <$> gRemoveField @(n-1) b)

class GInsertField (n :: Nat) f where
  gInsertField :: RemoveField n f x -> FieldTypeAt n f -> f x

instance GInsertField n f => GInsertField n (M1 i c f) where
  gInsertField (M1 a) t = M1 (gInsertField @n a t)

instance (If (n == 0) (() :: Constraint) (GInsertField (n-1) g), IsBool (n == 0))
  => GInsertField n (M1 s m (K1 i t) :*: g) where
  gInsertField ab t = _If @(n == 0)
    (M1 (K1 t) :*: ab)
    (let a :*: b = ab in a :*: gInsertField @(n-1) b t)

type family   ConstrAt (n :: Nat) (f :: k -> *) :: k -> *
type instance ConstrAt n (M1 i m f) = ConstrAt n f
type instance ConstrAt n (f :+: g) = If (n == 0) f (ConstrAt (n-1) g)

type family   RemoveConstr (n :: Nat) (f :: k -> *) :: k -> *
type instance RemoveConstr n (M1 i m f) = M1 i m (RemoveConstr n f)
type instance RemoveConstr n (f :+: g) = If (n == 0) g (f :+: RemoveConstr (n-1) g)

class GRemoveConstr (n :: Nat) f where
  gRemoveConstr :: f x -> Either (ConstrAt n f x) (RemoveConstr n f x)

instance GRemoveConstr n f => GRemoveConstr n (M1 i c f) where
  gRemoveConstr (M1 a) = M1 <$> gRemoveConstr @n a

instance (If (n == 0) (() :: Constraint) (GRemoveConstr (n-1) g), IsBool (n == 0))
  => GRemoveConstr n (f :+: g) where
  gRemoveConstr = _If @(n == 0)
    (\case
      L1 a -> Left a
      R1 b -> Right b)
    (\case
      L1 a -> Right (L1 a)
      R1 b -> R1 <$> gRemoveConstr @(n-1) b)

class GInsertConstr (n :: Nat) f where
  gInsertConstr :: Either (ConstrAt n f x) (RemoveConstr n f x) -> f x

instance GInsertConstr n f => GInsertConstr n (M1 i c f) where
  gInsertConstr = M1 . gInsertConstr @n . fmap unM1

instance (If (n == 0) (() :: Constraint) (GInsertConstr (n-1) g), IsBool (n == 0))
  => GInsertConstr n (f :+: g) where
  gInsertConstr = _If @(n == 0)
    (\case
      Left a -> L1 a
      Right b -> R1 b)
    (\case
      Left a -> R1 (gInsertConstr @(n-1) (Left a))
      Right (L1 a) -> L1 a
      Right (R1 b) -> R1 (gInsertConstr @(n-1) (Right b)))
