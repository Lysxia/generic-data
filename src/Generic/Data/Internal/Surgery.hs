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

-- | Operate on data types: insert\/modify\/delete fields and constructors.

module Generic.Data.Internal.Surgery where

import Control.Monad ((<=<))
import Data.Bifunctor (bimap, first)
import Data.Coerce
import Data.Kind (Constraint)
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits

import Fcf

import Generic.Data.Internal.Compat (Div)
import Generic.Data.Internal.Data
import Generic.Data.Internal.Meta (MetaOf, MetaConsName)

-- | /A sterile operating room, where generic data comes to be altered./
--
-- Generic representation in a list-of-lists shape at the type level
-- (reusing the constructors from "GHC.Generics" for convenience).
-- This representation makes it easy to modify fields and constructors.
--
-- We may also refer to the representation @l@ as a "row" of constructors or of
-- (unnamed or record) fields, depending on whether it represents a sum type.
--
-- @x@ corresponds to the last parameter of 'Rep', and is currently ignored by
-- this module (no support for 'Generic1').
newtype OR l x = OR { unOR :: l x }

-- | /Move fresh data to the operating room, where surgeries can be applied./
--
-- Convert a generic type to a list-of-lists-shaped representation.
toOR :: forall a l x. (Generic a, ToORRep a l) => a -> OR l x
toOR = OR . gLinearize . from

-- | /Move altered data out of the operating room, to be consumed by/
-- /some generic function./
--
-- Convert a list-of-lists representation to a synthetic generic type.
--
-- The synthesized representation is made of balanced binary trees,
-- corresponding closely to what GHC would generate for an actual data type.
--
-- That structure assumed by at least one piece of code out there (@aeson@).
toData :: forall f l x. FromOR f l => OR l x -> Data f x
toData = Data . gArborify . unOR

-- | /Move altered data, produced by some generic function, to the operating/
-- /room./
--
-- The inverse of 'toData'.
fromData :: forall f l x. ToOR f l => Data f x -> OR l x
fromData = OR . gLinearize . unData

-- | /Move restored data out of the operating room and back to the real/
-- /world./
--
-- The inverse of 'toOR'.
--
-- It may be useful to annotate the output type of 'fromOR',
-- since the rest of the type depends on it and it might only be inferred from
-- the context. The following annotations are possible:
--
-- @
-- 'fromOR' :: 'OROf' a -> a
-- 'fromOR' \@a  -- with TypeApplications
-- @
fromOR :: forall a l x. (Generic a, FromORRep a l) => OR l x -> a
fromOR = to . gArborify . unOR

-- | The list-of-lists generic representation type of type @a@,
-- that 'toOR' and 'fromOR' convert to and from.
type OROf a = OR (Linearize (Rep a)) ()

-- | This constraint means that @a@ is convertible /to/ its list-of-lists
-- generic representation. Implies @'OROf' a ~ 'OR' l ()@.
type   ToORRep a l =   ToOR (Rep a) l

-- | This constraint means that @a@ is convertible /from/ its list-of-lists
-- generic representation. Implies @'OROf' a ~ 'OR' l ()@.
type FromORRep a l = FromOR (Rep a) l

-- | Similar to 'ToORRep', but as a constraint on the standard
-- generic representation of @a@ directly, @f ~ 'Rep' a@.
type   ToOR f l = (GLinearize f, Linearize f ~ l, f ~ Arborify l)

-- | Similar to 'FromORRep', but as a constraint on the standard
-- generic representation of @a@ directly, @f ~ 'Rep' a@.
type FromOR f l = (GArborify  f, Linearize f ~ l, f ~ Arborify l)

--

-- | @'removeCField' \@n \@t@: remove the @n@-th field of type @t@
-- in a non-record single-constructor type.
removeCField
  :: forall    n t lt l x
  .  RmvCField n t lt l
  => OR lt x -> (t, OR l x)
removeCField (OR a) = OR <$> gRemoveField @n a

-- | @'removeRField' \@\"fdName\" \@n \@t@: remove the field @fdName@
-- at position @n@ of type @t@ in a record type.
removeRField
  :: forall    fd n t lt l x
  .  RmvRField fd n t lt l
  => OR lt x -> (t, OR l x)
removeRField (OR a) = OR <$> gRemoveField @n a

-- | @'insertCField' \@n \@t@: insert a field of type @t@
-- at position @n@ in a non-record single-constructor type.
insertCField
  :: forall    n t lt l x
  .  InsCField n t lt l
  => t -> OR l x -> OR lt x
insertCField z (OR a) = OR (gInsertField @n z a)

-- | @'insertRField' \@\"fdName\" \@n \@t@: insert a field
-- named @fdName@ of type @t@ at position @n@ in a record type.
insertRField
  :: forall    fd n t lt l x
  .  InsRField fd n t lt l
  => t -> OR l x -> OR lt x
insertRField z (OR a) = OR (gInsertField @n z a)

-- | @'removeConstr' \@\"C\" \@n \@t@: remove the @n@-th constructor, named @C@,
-- with contents isomorphic to the tuple @t@.
--
-- @()@ and 'Data.Functor.Identity.Identity' can be used as an empty and a
-- singleton tuple.
removeConstr
  :: forall    c t n lc l l_t x
  .  RmvConstr c t n lc l l_t x
  => OR lc x -> Either t (OR l x)
removeConstr (OR a) = bimap
  (to . coerce' . gArborify @(Arborify l_t)) OR (gRemoveConstr @n a)

-- | @'insertConstr' \@\"C\" \@n \@t@: insert a constructor @C@ at position @n@
-- with contents isomorphic to the tuple @t@.
--
-- @()@ and 'Data.Functor.Identity.Identity' can be used as an empty and a
-- singleton tuple.
insertConstr
  :: forall    c t n lc l l_t x
  .  InsConstr c t n lc l l_t x
  => Either t (OR l x) -> OR lc x
insertConstr z =
  OR (gInsertConstr @n
    (bimap (gLinearize @(Arborify l_t) . coerce' . from) unOR z))

--

-- | This constraint means that the (unnamed) field row @lt@ contains
-- a field of type @t@ at position @n@, and removing it yields row @l@.
type RmvCField n t lt l =
  ( GRemoveField n lt
  , CFieldSurgery n t lt l
  )

-- | This constraint means that the record field row @lt@ contains a field of
-- type @t@ named @fd@ at position @n@, and removing it yields row @l@.
type RmvRField fd n t lt l =
  ( GRemoveField n lt
  , RFieldSurgery fd n t lt l
  )

-- | This constraint means that inserting a field @t@ at position @n@ in the
-- (unnamed) field row @t@ yields row @lt@.
type InsCField n t lt l =
  ( GInsertField n lt
  , CFieldSurgery n t lt l
  )

-- | This constraint means that inserting a field @t@ named @fd@ at position
-- @n@ in the record field row @t@ yields row @lt@.
type InsRField fd n t lt l =
  ( GInsertField n lt
  , RFieldSurgery fd n t lt l
  )

-- | This constraint means that the constructor row @lc@ contains a constructor
-- named @c@ at position @n@, and removing it from @lc@ yields row @l@.
-- Furthermore, constructor @c@ contains a field row @l_t@ compatible with the
-- tuple type @t@.
type RmvConstr c t n lc l l_t x =
  ( GRemoveConstr n lc
  , GArborify (Arborify l_t)
  , Coercible (Arborify l_t x) (Rep t x)  -- Coercible is... (contd.)
  , ConstrSurgery c t n lc l l_t x
  )

-- | This constraint means that the inserting a constructor @c@ at position @n@
-- in the constructor row @l@ yields row @lc@.
-- Furthermore, constructor @c@ contains a field row @l_t@ compatible with the
-- tuple type @t@.
type InsConstr c t n lc l l_t x =
  ( GInsertConstr n lc
  , GLinearize (Arborify l_t)
  , Coercible (Rep t x) (Arborify l_t x)  -- ... not symmetric?
  , ConstrSurgery c t n lc l l_t x
  )

type FieldSurgery n t lt l =
  ( t ~ FieldTypeAt n lt
  , l ~ RemoveField n lt
  )

type CFieldSurgery n t lt l =
  ( lt ~ InsertField n 'Nothing t l
  , FieldSurgery n t lt l
  )

type RFieldSurgery fd n t lt l =
  ( n ~ FieldIndex fd lt
  , lt ~ InsertField n ('Just fd) t l
  , FieldSurgery n t lt l
  )

type ConstrSurgery c t n lc l l_t x =
  ( Generic t
  , MatchFields (UnM1 (Rep t)) (Arborify l_t)
  , n ~ ConstrIndex c lc
  , c ~ MetaConsName (MetaOf l_t)
  , l_t ~ Linearize (Arborify l_t)
  , l_t ~ ConstrAt n lc
  , lc ~ InsertConstr n l_t l
  , l ~ RemoveConstr n lc
  )

--

coerce' :: Coercible (f x) (g x) => f x -> g x
coerce' = coerce

absurd :: V1 x -> a
absurd !_ = error "impossible"

type DummyMeta = 'MetaData "" "" "" 'False

type family   UnM1 (f :: k -> *) :: k -> *
type instance UnM1 (M1 i c f) = f

--

type family   Linearize (f :: k -> *) :: k -> *
type instance Linearize (M1 D m f) = M1 D m (LinearizeSum f V1)
type instance Linearize (M1 C m f) = M1 C m (LinearizeProduct f U1)

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

instance GLinearizeSum f V1 => GLinearize (M1 D m f) where
  gLinearize (M1 a) = M1 (gLinearizeSum @_ @V1 (Left a))

instance GLinearizeProduct f U1 => GLinearize (M1 C m f) where
  gLinearize (M1 a) = M1 (gLinearizeProduct a U1)

class GLinearizeSum f tl where
  gLinearizeSum :: Either (f x) (tl x) -> LinearizeSum f tl x

instance GLinearizeSum V1 tl where
  gLinearizeSum (Left  v) = absurd v
  gLinearizeSum (Right c) = c

instance (GLinearizeSum g tl, GLinearizeSum f (LinearizeSum g tl))
  => GLinearizeSum (f :+: g) tl where
  gLinearizeSum (Left (L1 a)) = gLinearizeSum @_ @(LinearizeSum g tl) (Left a)
  gLinearizeSum (Left (R1 b)) = gLinearizeSum @f (Right (gLinearizeSum @g @tl (Left b)))
  gLinearizeSum (Right c) = gLinearizeSum @f (Right (gLinearizeSum @g (Right c)))

instance GLinearizeProduct f U1 => GLinearizeSum (M1 c m f) tl where
  gLinearizeSum (Left (M1 a)) = L1 (M1 (gLinearizeProduct a U1))
  gLinearizeSum (Right c) = R1 c

class GLinearizeProduct f tl where
  gLinearizeProduct :: f x -> tl x -> LinearizeProduct f tl x

instance GLinearizeProduct U1 tl where
  gLinearizeProduct _ = id

instance (GLinearizeProduct g tl, GLinearizeProduct f (LinearizeProduct g tl))
  => GLinearizeProduct (f :*: g) tl where
  gLinearizeProduct (a :*: b) = gLinearizeProduct a . gLinearizeProduct b

instance GLinearizeProduct (M1 s m f) tl where
  gLinearizeProduct = (:*:)

class GArborify f where
  gArborify :: Linearize f x -> f x

instance GArborifySum f V1 => GArborify (M1 D m f) where
  gArborify (M1 a) = case gArborifySum @_ @V1 a of
    Left a' -> M1 a'
    Right v -> absurd v

instance GArborifyProduct f U1 => GArborify (M1 C m f) where
  gArborify (M1 a) = M1 (fst (gArborifyProduct @_ @U1 a))

class GArborifySum f tl where
  gArborifySum :: LinearizeSum f tl x -> Either (f x) (tl x)

instance GArborifySum V1 tl where
  gArborifySum = Right

instance (GArborifySum g tl, GArborifySum f (LinearizeSum g tl))
  => GArborifySum (f :+: g) tl where
  gArborifySum = first R1 . gArborifySum <=< first L1 . gArborifySum

instance GArborifyProduct f U1 => GArborifySum (M1 c m f) tl where
  gArborifySum (L1 (M1 a)) = Left (M1 (fst (gArborifyProduct @_ @U1 a)))
  gArborifySum (R1 c) = Right c

class GArborifyProduct f tl where
  gArborifyProduct :: LinearizeProduct f tl x -> (f x, tl x)

instance GArborifyProduct U1 tl where
  gArborifyProduct c = (U1, c)

instance (GArborifyProduct g tl, GArborifyProduct f (LinearizeProduct g tl))
  => GArborifyProduct (f :*: g) tl where
  gArborifyProduct abc = (a :*: b, c) where
    (a, bc) = gArborifyProduct abc
    (b,  c) = gArborifyProduct  bc

instance GArborifyProduct (M1 s m f) tl where
  gArborifyProduct (a :*: c) = (a, c)

type family   Arborify (f :: k -> *) :: k -> *
type instance Arborify (M1 D m f) = M1 D m (Eval (ArborifySum (CoArity f) f))
type instance Arborify (M1 C m f) = M1 C m (Eval (ArborifyProduct (Arity f) f))

data ArborifySum (n :: Nat) (f :: k -> *) :: (k -> *) -> *
type instance Eval (ArborifySum n V1) = V1
type instance Eval (ArborifySum n (f :+: g)) =
  Eval (If (n == 1)
    (ArborifyProduct (Arity f) f)
    (Arborify' ArborifySum (:+:) n (Div n 2) f g))

data ArborifyProduct (n :: Nat) (f :: k -> *) :: (k -> *) -> *
type instance Eval (ArborifyProduct n (M1 C s f)) = M1 C s (Eval (ArborifyProduct n f))
type instance Eval (ArborifyProduct n U1) = U1
type instance Eval (ArborifyProduct n (f :*: g)) =
  Eval (If (n == 1)
    (Pure f)
    (Arborify' ArborifyProduct (:*:) n (Div n 2) f g))

-- let nDiv2 = Div n 2 in ...
type Arborify' arb op n nDiv2 f g =
   (   Uncurry (Pure2 op)
   <=< BimapPair (arb nDiv2) (arb (n-nDiv2))
   <=< SplitAt nDiv2
   ) (op f g)

data SplitAt :: Nat -> (k -> *) -> (k -> *, k -> *) -> *
type instance Eval (SplitAt n (f :+: g)) =
  Eval (If (n == 0)
    (Pure '(V1, f :+: g))
    (BimapPair (Pure2 (:+:) f) Pure =<< SplitAt (n-1) g))
type instance Eval (SplitAt n (f :*: g)) =
  Eval (If (n == 0)
    (Pure '(U1, f :*: g))
    (BimapPair (Pure2 (:*:) f) Pure =<< SplitAt (n-1) g))

type family   FieldTypeAt (n :: Nat) (f :: k -> *) :: *
type instance FieldTypeAt n (M1 i c f) = FieldTypeAt n f
type instance FieldTypeAt n (f :+: V1) = FieldTypeAt n f
type instance FieldTypeAt n (f :*: g) = If (n == 0) (FieldTypeOf f) (FieldTypeAt (n-1) g)

type family   FieldTypeOf (f :: k -> *) :: *
type instance FieldTypeOf (M1 s m (K1 i a)) = a

type family   RemoveField (n :: Nat) (f :: k -> *) :: k -> *
type instance RemoveField n (M1 i m f) = M1 i m (RemoveField n f)
type instance RemoveField n (f :+: V1) = RemoveField n f :+: V1
type instance RemoveField n (f :*: g) = If (n == 0) g (f :*: RemoveField (n-1) g)

type DefaultMetaSel field
  = 'MetaSel field 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy

type family   InsertField (n :: Nat) (fd :: Maybe Symbol) (t :: *) (f :: k -> *) :: k -> *
type instance InsertField n fd t (M1 D m f) = M1 D m (InsertField n fd t f)
type instance InsertField n fd t (M1 C m f) = M1 C m (InsertField n fd t f)
type instance InsertField n fd t (f :+: V1) = InsertField n fd t f :+: V1
type instance InsertField n fd t (f :*: g) =
  If (n == 0)
    (M1 S (DefaultMetaSel fd) (K1 R t) :*: (f :*: g))
    (f :*: InsertField (n-1) fd t g)
type instance InsertField 0 fd t U1 = M1 S (DefaultMetaSel fd) (K1 R t) :*: U1

-- | Position of a record field
type family   FieldIndex (field :: Symbol) (f :: k -> *) :: Nat
type instance FieldIndex field (M1 D m f) = FieldIndex field f
type instance FieldIndex field (M1 C m f) = FieldIndex field f
type instance FieldIndex field (f :+: V1) = FieldIndex field f
type instance FieldIndex field (M1 S ('MetaSel ('Just field') su ss ds) f :*: g)
  = If (field == field') 0 (1 + FieldIndex field g)

-- | Number of fields of a single constructor
type family   Arity (f :: k -> *) :: Nat
type instance Arity (M1 d m f) = Arity f
type instance Arity (f :+: V1) = Arity f
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

instance GRemoveField n f => GRemoveField n (f :+: V1) where
  gRemoveField (L1 a) = L1 <$> gRemoveField @n a
  gRemoveField (R1 v) = absurd v

instance (If (n == 0) (() :: Constraint) (GRemoveField (n-1) g), IsBool (n == 0))
  => GRemoveField n (M1 s m (K1 i t) :*: g) where
  gRemoveField (a@(M1 (K1 t)) :*: b) = _If @(n == 0)
    (t, b)
    ((a :*:) <$> gRemoveField @(n-1) b)

class GInsertField (n :: Nat) f where
  gInsertField :: FieldTypeAt n f -> RemoveField n f x -> f x

instance GInsertField n f => GInsertField n (M1 i c f) where
  gInsertField t (M1 a) = M1 (gInsertField @n t a)

instance GInsertField n f => GInsertField n (f :+: V1) where
  gInsertField t (L1 a) = L1 (gInsertField @n t a)
  gInsertField _ (R1 v) = absurd v

instance (If (n == 0) (() :: Constraint) (GInsertField (n-1) g), IsBool (n == 0))
  => GInsertField n (M1 s m (K1 i t) :*: g) where
  gInsertField t ab = _If @(n == 0)
    (M1 (K1 t) :*: ab)
    (let a :*: b = ab in a :*: gInsertField @(n-1) t b)

type family   ConstrAt (n :: Nat) (f :: k -> *) :: k -> *
type instance ConstrAt n (M1 i m f) = ConstrAt n f
type instance ConstrAt n (f :+: g) = If (n == 0) f (ConstrAt (n-1) g)

type family   RemoveConstr (n :: Nat) (f :: k -> *) :: k -> *
type instance RemoveConstr n (M1 i m f) = M1 i m (RemoveConstr n f)
type instance RemoveConstr n (f :+: g) = If (n == 0) g (f :+: RemoveConstr (n-1) g)

type family   InsertConstr (n :: Nat) (t :: k -> *) (f :: k -> *) :: k -> *
type instance InsertConstr n t (M1 i m f) = M1 i m (InsertConstr n t f)
type instance InsertConstr n t (f :+: g) =
  If (n == 0) (t :+: (f :+: g)) (f :+: InsertConstr (n-1) t g)
type instance InsertConstr 0 t V1 = t :+: V1

type family   ConstrIndex (con :: Symbol) (f :: k -> *) :: Nat
type instance ConstrIndex con (M1 D m f) = ConstrIndex con f
type instance ConstrIndex con (M1 C ('MetaCons con' fx s) f :+: g) =
  If (con == con') 0 (1 + ConstrIndex con g)

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

-- | Generate equality constraints between fields of two matching generic
-- representations.
class MatchFields (f :: k -> *) (g :: k -> *)
instance (g' ~ M1 D d g, MatchFields f g) => MatchFields (M1 D c f) g'
-- Forcing the MetaCons field
instance (g' ~ M1 C ('MetaCons _cn _s _t) g, MatchFields f g)
  => MatchFields (M1 C c f) g'
instance (g' ~ M1 S d g, MatchFields f g) => MatchFields (M1 S c f) g'
instance (g' ~ (g1 :+: g2), MatchFields f1 g1, MatchFields f2 g2)
  => MatchFields (f1 :+: f2) g'
instance (g' ~ (g1 :*: g2), MatchFields f1 g1, MatchFields f2 g2)
  => MatchFields (f1 :*: f2) g'
instance (g' ~ K1 j a) => MatchFields (K1 i a) g'
instance (g' ~ U1) => MatchFields U1 g'
instance (g' ~ V1) => MatchFields V1 g'
