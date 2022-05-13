{-# LANGUAGE
    AllowAmbiguousTypes,
    DataKinds,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    PolyKinds,
    ScopedTypeVariables,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

-- | Surgeries that are just 'coerce'.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Microsurgery where

import Data.Coerce (Coercible, coerce)
import GHC.Generics
import GHC.TypeLits (ErrorMessage(..), Symbol, TypeError)

import Generic.Data.Types (Data)
import Generic.Data.Internal.Generically (Generically(..), GenericProduct(..))

-- * Surgery

-- | Apply a microsurgery @s@ to a type @a@ for @DerivingVia@.
--
-- For the 'Data.Monoid.Monoid' class, see 'ProductSurgery'.
--
-- === Example
--
-- @
-- {-\# LANGUAGE DerivingVia \#-}
--
-- -- The constructors must be visible.
-- import "Generic.Data.Microsurgery"
--   ('Surgery', 'Surgery''(..), 'Generically'(..), 'Derecordify')
--
-- data T = T { unT :: Int }
--   deriving 'Show' via ('Surgery' 'Derecordify' T)
--
-- -- T won't be shown as a record:
-- --   show (T {unT = 3}) == "T 3"
-- @
type Surgery (s :: *) (a :: *) = Generically (Surgery' s a)

-- | Apply a microsurgery @s@ to a type @a@ for @DerivingVia@ for the
-- 'Data.Monoid.Monoid' class.
type ProductSurgery (s :: *) (a :: *) = GenericProduct (Surgery' s a)

-- | Plural of 'Surgery'. Apply a list of microsurgeries.
type Surgeries (s :: [*]) (a :: *) = Surgery (Cat s) a

-- | Plural of 'ProductSurgery'. Apply a list of microsurgeries.
type ProductSurgeries (s :: [*]) (a :: *) = ProductSurgery (Cat s) a

-- | See 'Surgery'.
newtype Surgery' (s :: *) (a :: *) = Surgery' { unSurgery' :: a }

instance (Generic a, Coercible (GSurgery s (Rep a)) (Rep a)) => Generic (Surgery' s a) where
  type Rep (Surgery' s a) = GSurgery s (Rep a)
  from = (coerce :: forall x. (a -> Rep a x) -> Surgery' s a -> GSurgery s (Rep a) x) from
  to = (coerce :: forall x. (Rep a x -> a) -> GSurgery s (Rep a) x -> Surgery' s a) to

-- | Apply a microsurgery represented by a symbol @s@ (declared as a dummy data
-- type) to a generic representation @f@.
type family GSurgery (s :: *) (f :: k -> *) :: k -> *

-- * Derecordify

derecordify ::
  Coercible (GSurgery Derecordify f) f =>
  -- Coercible is not symmetric!??
  Data f p -> Data (GSurgery Derecordify f) p
derecordify = coerce

underecordify ::
  Coercible f (GSurgery Derecordify f) =>
  Data (GSurgery Derecordify f) p -> Data f p
underecordify = coerce

-- | Forget that a type was declared using record syntax.
--
-- > data Foo = Bar { baz :: Zap }
-- >
-- > -- becomes --
-- >
-- > data Foo = Bar Zap
--
-- Concretely, set the last field of 'MetaCons' to 'False' and forget field
-- names.
--
-- This is a defunctionalized symbol, applied using 'GSurgery' or 'Surgery'.
data Derecordify :: *
type instance GSurgery Derecordify f = GDerecordify f

type family GDerecordify (f :: k -> *) :: k -> *
type instance GDerecordify (M1 D m f) = M1 D m (GDerecordify f)
type instance GDerecordify (f :+: g) = GDerecordify f :+: GDerecordify g
type instance GDerecordify (f :*: g) = GDerecordify f :*: GDerecordify g
type instance GDerecordify (M1 C ('MetaCons nm fx _isRecord) f) = M1 C ('MetaCons nm fx 'False) (GDerecordify f)
type instance GDerecordify (M1 S ('MetaSel _nm su ss ds) f) = M1 S ('MetaSel 'Nothing su ss ds) f
type instance GDerecordify V1 = V1
type instance GDerecordify U1 = U1

-- * Type aging ("denewtypify")

typeage ::
  Coercible (GSurgery Typeage f) f =>
  Data f p -> Data (GSurgery Typeage f) p
typeage = coerce

untypeage ::
  Coercible f (GSurgery Typeage f) =>
  Data (GSurgery Typeage f) p -> Data f p
untypeage = coerce

-- | Forget that a type is a @newtype@. (The pun is that \"aging\" a type makes
-- it no longer \"new\".)
--
-- > newtype Foo = Bar Baz
-- >
-- > -- becomes --
-- >
-- > data Foo = Bar Baz
--
-- This is a defunctionalized symbol, applied using 'GSurgery' or 'Surgery'.
data Typeage :: *
type instance GSurgery Typeage (M1 D ('MetaData nm md pk _nt) f) = M1 D ('MetaData nm md pk 'False) f

-- * Renaming

renameFields ::
  forall rnm f p.
  Coercible (GSurgery (RenameFields rnm) f) f =>
  Data f p -> Data (GSurgery (RenameFields rnm) f) p
renameFields = coerce

unrenameFields ::
  forall rnm f p.
  Coercible (GSurgery (RenameFields rnm) f) f =>
  Data f p -> Data (GSurgery (RenameFields rnm) f) p
unrenameFields = coerce

renameConstrs ::
  forall rnm f p.
  Coercible (GSurgery (RenameConstrs rnm) f) f =>
  Data f p -> Data (GSurgery (RenameConstrs rnm) f) p
renameConstrs = coerce

unrenameConstrs ::
  forall rnm f p.
  Coercible (GSurgery (RenameConstrs rnm) f) f =>
  Data f p -> Data (GSurgery (RenameConstrs rnm) f) p
unrenameConstrs = coerce

-- | Rename fields using the function @rnm@ given as a parameter.
--
-- > data Foo = Bar { baz :: Zap }
-- >
-- > -- becomes, renaming "baz" to "bag" --
-- >
-- > data Foo = Bar { bag :: Zap }
--
-- This is a defunctionalized symbol, applied using 'GSurgery' or 'Surgery'.
data RenameFields (rnm :: *) :: *
type instance GSurgery (RenameFields rnm) f = GRenameFields rnm f

type family GRenameFields (rnm :: *) (f :: k -> *) :: k -> *
type instance GRenameFields rnm (M1 D m f) = M1 D m (GRenameFields rnm f)
type instance GRenameFields rnm (f :+: g) = GRenameFields rnm f :+: GRenameFields rnm g
type instance GRenameFields rnm (f :*: g) = GRenameFields rnm f :*: GRenameFields rnm g
type instance GRenameFields rnm (M1 C m f) = M1 C m (GRenameFields rnm f)
type instance GRenameFields rnm (M1 S ('MetaSel ('Just nm) su ss ds) f) = M1 S ('MetaSel ('Just (rnm @@ nm)) su ss ds) f
type instance GRenameFields rnm V1 = V1
type instance GRenameFields rnm U1 = U1

-- | Rename constructors using the function @rnm@ given as a parameter.
--
-- > data Foo = Bar { baz :: Zap }
-- >
-- > -- becomes, renaming "Bar" to "Car" --
-- >
-- > data Foo = Car { baz :: Zap }
--
-- This is a defunctionalized symbol, applied using 'GSurgery' or 'Surgery'.
data RenameConstrs (rnm :: *) :: *
type instance GSurgery (RenameConstrs rnm) f = GRenameConstrs rnm f

type family GRenameConstrs (rnm :: *) (f :: k -> *) :: k -> *
type instance GRenameConstrs rnm (M1 D m f) = M1 D m (GRenameConstrs rnm f)
type instance GRenameConstrs rnm (f :+: g) = GRenameConstrs rnm f :+: GRenameConstrs rnm g
type instance GRenameConstrs rnm (f :*: g) = GRenameConstrs rnm f :*: GRenameConstrs rnm g
type instance GRenameConstrs rnm (M1 C ('MetaCons nm fi ir) f) = M1 C ('MetaCons (rnm @@ nm) fi ir) f
type instance GRenameConstrs rnm V1 = V1

-- ** Defining symbol functions

-- | @f \@\@ s@ is the application of a type-level function symbolized by @f@
-- to a @s :: 'Symbol'@.
--
-- A function @FooToBar@ can be defined as follows:
--
-- @
-- data FooToBar
-- type instance FooToBar '@@' \"foo\" = \"bar\"
-- @
type family (f :: *) @@ (s :: Symbol) :: Symbol

-- | Identity function @'Symbol' -> 'Symbol'@.
data SId
type instance SId @@ s = s

-- | Empty function (compile-time error when applied).
data SError
type instance SError @@ s = TypeError ('Text "Invalid name: " ':<>: 'ShowType s)

-- | Constant function.
data SConst (s :: Symbol)
type instance SConst z @@ _s = z

-- | Define a function for a fixed set of strings, and fall back to @f@ for the others.
data SRename (xs :: [(Symbol, Symbol)]) (f :: *)
type instance SRename xs f @@ s = SRename' xs f s

-- | Closed type family for 'SRename'.
type family SRename' (xs :: [(Symbol, Symbol)]) (f :: *) (s :: Symbol) where
  SRename' '[] f s = f @@ s
  SRename' ('( s,  t) ': _xs) _f s = t
  SRename' ('(_r, _t) ':  xs)  f s = SRename' xs f s

-- * Other

-- This can be used with generic-lens (see Generic.Data.Microsurgery)

-- | Unify the "spines" of two generic representations (the "spine" is
-- everything except the field types).
class UnifyRep (f :: k -> *) (g :: k -> *)
instance (g' ~ M1 s c g, UnifyRep f g) => UnifyRep (M1 s c f) g'
instance (g' ~ (g1 :+: g2), UnifyRep f1 g1, UnifyRep f2 g2)
  => UnifyRep (f1 :+: f2) g'
instance (g' ~ (g1 :*: g2), UnifyRep f1 g1, UnifyRep f2 g2)
  => UnifyRep (f1 :*: f2) g'
instance (g' ~ K1 i b) => UnifyRep (K1 i a) g'
instance (g' ~ U1) => UnifyRep U1 g'
instance (g' ~ V1) => UnifyRep V1 g'

-- |
--
-- > onData :: _ => (Data r x -> Data s y) -> (Data r x -> Data s y)  -- possible specialization
--
-- Can be used with @generic-lens@ for type-changing field updates with @field_@
-- (and possibly other generic optics).
--
-- A specialization of the identity function to be used to fix types
-- of functions on 'Data', unifying the "spines" of input and output generic
-- representations (the "spine" is everything except field types, which may
-- thus change).
onData
  :: (UnifyRep r s, UnifyRep s r)
  => p (Data r x) (Data s y) -> p (Data r x) (Data s y)
onData = id

-- | Apply a type constructor @f@ to every field type of a generic
-- representation @r@.
--
-- > data Color = RGB
-- >   { r :: Int
-- >   , g :: Int
-- >   , b :: Int }
-- >
-- > -- becomes --
-- >
-- > data Color f = RGB
-- >   { r :: f Int
-- >   , g :: f Int
-- >   , b :: f Int }
--
-- This is a defunctionalized symbol, applied using 'GSurgery' or 'Surgery'.
data OnFields (f :: * -> *) :: *
type instance GSurgery (OnFields f) g = GOnFields f g

type family GOnFields (f :: * -> *) (g :: k -> *) :: k -> *
type instance GOnFields f (M1 s m r) = M1 s m (GOnFields f r)
type instance GOnFields f (r :+: s) = GOnFields f r :+: GOnFields f s
type instance GOnFields f (r :*: s) = GOnFields f r :*: GOnFields f s
type instance GOnFields f (K1 i a) = K1 i (f a)
type instance GOnFields f U1 = U1
type instance GOnFields f V1 = V1

-- | Apply a type constructor @f@ to every field type of a type @a@ to make a
-- synthetic type.
type DOnFields (f :: * -> *) (a :: *) = Data (GSurgery (OnFields f) (Rep a)) ()

-- | Apply a type constructor @f@ to the field named @s@ in a generic record @r@.
--
-- > data Vec a = Vec
-- >   { len :: Int
-- >   , contents :: [a] }
-- >
-- > -- with (OnField "len" Sum) becomes --
-- >
-- > data Vec a = Vec
-- >   { len :: Sum Int
-- >   , contents :: [a] }
--
-- This is a defunctionalized symbol, applied using 'GSurgery' or 'Surgery'.
-- See also the synonym @('%~')@.
data OnField (s :: Symbol) (f :: * -> *) :: *
type instance GSurgery (OnField s f) g = GOnField s f g

type family GOnField (x :: Symbol) (f :: * -> *) (g :: k -> *) :: k -> * where
  GOnField x f (M1 S ('MetaSel ('Just x) a b c) (K1 i t)) = M1 S ('MetaSel ('Just x) a b c) (K1 i (f t))
  GOnField x f (M1 S m r) = M1 S m r
  GOnField x f (M1 C m r) = M1 C m (GOnField x f r)
  GOnField x f (M1 D m r) = M1 D m (GOnField x f r)
  GOnField x f (r :+: s) = GOnField x f r :+: GOnField x f s
  GOnField x f (r :*: s) = GOnField x f r :*: GOnField x f s
  GOnField x f (K1 i a) = K1 i (f a)
  GOnField x f U1 = U1
  GOnField x f V1 = V1

-- | Infix name for 'OnField'. To be used with 'Surgeries' or 'Cat'.
--
-- === __Examples__
--
-- Transform one @Int@ field into @'Data.Monoid.Sum' Int@ for deriving 'Monoid':
--
-- @
-- data Vec a = Vec
--   { len :: Int
--   , contents :: [a] }
--   deriving Generic
--   deriving (Eq, Show) via Generically (Vec a)
--   deriving (Semigroup, Monoid) via 'ProductSurgeries' '[\"len\" '%~' 'Data.Monoid.Sum'] (Vec a)
-- @
--
-- Wrap unshowable fields in 'Generic.Data.Opaque' for deriving 'Show':
--
-- @
-- data Unshowable = Unshowable
--   { fun :: Int -> Int
--   , io :: IO Bool
--   , int :: Int }
--   deriving Generic
--   deriving Show via 'Surgeries' '[\"fun\" '%~' 'Generic.Data.Opaque', \"io\" '%~' 'Generic.Data.Opaque'] Unshowable
--
-- -- show (Unshowable id (pure True) 42) = \"Unshowable _ _ 42\"
-- @
type (%~) = OnField
infixr 4 %~

-- | Compose surgeries together.
data Cat (ss :: [*]) :: *
type instance GSurgery (Cat '[]) g = g
type instance GSurgery (Cat (s ': ss)) g = GSurgery s (GSurgery (Cat ss) g)

-- | Make a synthetic type ('Data') by chaining multiple surgeries.
type DCat (ss :: [*]) (a :: *) = Data (GSurgery (Cat ss) (Rep a)) ()

-- | Change the generic representation to that of another type @a@.
data CopyRep (a :: *) :: *
type instance GSurgery (CopyRep a) _ = Rep a

copyRep :: forall a f p.
  Coercible (GSurgery (CopyRep a) f) f =>
  Data f p -> Data (GSurgery (CopyRep a) f) p
copyRep = coerce

uncopyRep :: forall a f p.
  Coercible f (GSurgery (CopyRep a) f) =>
  Data (GSurgery (CopyRep a) f) p -> Data f p
uncopyRep = coerce
