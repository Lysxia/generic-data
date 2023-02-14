{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type metadata accessors
--
-- Type names, constructor names...
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Meta where

import Data.Proxy
import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (Symbol, Nat, KnownNat, type (+), natVal, TypeError, ErrorMessage(..))

import Generic.Data.Internal.Functions

-- $setup
-- >>> :set -XDataKinds -XTypeApplications
-- >>> import Control.Applicative (ZipList)
-- >>> import Data.Monoid (Sum(..))

-- | Name of the first data constructor in a type as a string.
--
-- >>> gdatatypeName @(Maybe Int)
-- "Maybe"
gdatatypeName :: forall a. (Generic a, GDatatype (Rep a)) => String
gdatatypeName = gDatatypeName @(Rep a)

-- | Name of the module where the first type constructor is defined.
--
-- >>> gmoduleName @(ZipList Int)
-- "Control.Applicative"
gmoduleName :: forall a. (Generic a, GDatatype (Rep a)) => String
gmoduleName = gModuleName @(Rep a)

-- | Name of the package where the first type constructor is defined.
--
-- >>> gpackageName @(Maybe Int)
-- "base"
gpackageName :: forall a. (Generic a, GDatatype (Rep a)) => String
gpackageName = gPackageName @(Rep a)

-- | 'True' if the first type constructor is a newtype.
--
-- >>> gisNewtype @[Int]
-- False
-- >>> gisNewtype @(ZipList Int)
-- True
gisNewtype :: forall a. (Generic a, GDatatype (Rep a)) => Bool
gisNewtype = gIsNewtype @(Rep a)

fromDatatype :: forall d r. Datatype d => (M1 D d Proxy () -> r) -> r
fromDatatype f = f (M1 Proxy :: M1 D d Proxy ())

-- | Generic representations that contain datatype metadata.
class GDatatype f where
  gDatatypeName :: String
  gModuleName :: String
  gPackageName :: String
  gIsNewtype :: Bool

instance Datatype d => GDatatype (M1 D d f) where
  gDatatypeName = fromDatatype @d datatypeName
  gModuleName = fromDatatype @d moduleName
  gPackageName = fromDatatype @d packageName
  gIsNewtype = fromDatatype @d isNewtype

-- | Name of the first constructor in a value.
--
-- >>> gconName (Just 0)
-- "Just"
gconName :: forall a. Constructors a => a -> String
gconName = conIdToString . conId

-- | The fixity of the first constructor.
--
-- >>> import GHC.Generics ((:*:)(..))
-- >>> gconFixity (Just 0)
-- Prefix
-- >>> gconFixity ([] :*: id)
-- Infix RightAssociative 6
gconFixity :: forall a. Constructors a => a -> Fixity
gconFixity = gConFixity . from

-- | 'True' if the constructor is a record.
--
-- >>> gconIsRecord (Just 0)
-- False
-- >>> gconIsRecord (Sum 0)   -- Note:  newtype Sum a = Sum { getSum :: a }
-- True
gconIsRecord :: forall a. Constructors a => a -> Bool
gconIsRecord = gConIsRecord . from

-- | Number of constructors.
--
-- >>> gconNum @(Maybe Int)
-- 2
gconNum :: forall a. Constructors a => Int
gconNum = gConNum @(Rep a)

-- | Index of a constructor.
--
-- >>> gconIndex Nothing
-- 0
-- >>> gconIndex (Just "test")
-- 1
gconIndex :: forall a. Constructors a => a -> Int
gconIndex = conIdToInt . conId

-- | An opaque identifier for a constructor.
newtype ConId a = ConId Int
  deriving (Eq, Ord, Show)

-- | Identifier of a constructor.
conId :: forall a. Constructors a => a -> ConId a
conId = toConId . gConId . from

-- | Index of a constructor, given its identifier.
-- See also 'gconIndex'.
conIdToInt :: forall a. ConId a -> Int
conIdToInt (ConId i) = i

-- | Name of a constructor. See also 'gconName'.
conIdToString :: forall a. Constructors a => ConId a -> String
conIdToString = gConIdToString . fromConId

-- | All constructor identifiers.
--
-- @
-- 'gconNum' \@a = length ('conIdEnum' \@a)
-- @
conIdEnum :: forall a. Constructors a => [ConId a]
conIdEnum = fmap ConId [0 .. n-1]
  where
    n = gConNum @(Rep a)

-- | The first constructor. This must not be called on an empty type.
conIdMin :: forall a. (Constructors a, NonEmptyType "conIdMin" a) => ConId a
conIdMin = ConId 0

-- | The last constructor. This must not be called on an empty type.
conIdMax :: forall a. (Constructors a, NonEmptyType "conIdMax" a) => ConId a
conIdMax = toConId gConIdMax

-- | Get a 'ConId' by name.
--
-- >>> conIdNamed @"Nothing" :: ConId (Maybe Int)
-- ConId 0
-- >>> conIdNamed @"Just"    :: ConId (Maybe Int)
-- ConId 1
conIdNamed :: forall s a. ConIdNamed s a => ConId a
conIdNamed = ConId (fromInteger (natVal (Proxy @(ConIdNamed' s a))))

-- | Constraint synonym for 'Generic' and 'GConstructors'.
class (Generic a, GConstructors (Rep a)) => Constructors a
instance (Generic a, GConstructors (Rep a)) => Constructors a

-- | Constraint synonym for generic types @a@ with a constructor named @n@.
class (Generic a, KnownNat (ConIdNamed' n a)) => ConIdNamed n a
instance (Generic a, KnownNat (ConIdNamed' n a)) => ConIdNamed n a

-- *** Constructor information on generic representations

newtype GConId r = GConId Int
  deriving (Eq, Ord)

gConIdToInt :: GConId r -> Int
gConIdToInt (GConId i) = i

toConId :: forall a. Generic a => GConId (Rep a) -> ConId a
toConId (GConId i) = ConId i

fromConId :: forall a. Generic a => ConId a -> GConId (Rep a)
fromConId (ConId i) = GConId i

reGConId :: GConId r -> GConId s
reGConId (GConId i) = GConId i

gConIdMin :: forall r. GConstructors r => GConId r
gConIdMin = GConId 0

gConIdMax :: forall r. GConstructors r => GConId r
gConIdMax = GConId (gConNum @r - 1)

-- | Generic representations that contain constructor metadata.
class GConstructors r where
  gConIdToString :: GConId r -> String
  gConId :: r p -> GConId r
  gConNum :: Int
  gConFixity :: r p -> Fixity
  gConIsRecord :: r p -> Bool

instance GConstructors f => GConstructors (M1 D c f) where
  gConIdToString = gConIdToString @f . reGConId
  gConId = reGConId . gConId . unM1
  gConNum = gConNum @f
  gConFixity = gConFixity . unM1
  gConIsRecord = gConIsRecord . unM1

instance (GConstructors f, GConstructors g) => GConstructors (f :+: g) where
  gConIdToString (GConId i) =
    if i < nf then
      gConIdToString @f (GConId i)
    else
      gConIdToString @g (GConId (i - nf))
    where
      nf = gConNum @f
  gConId (L1 x) = reGConId (gConId x)
  gConId (R1 y) = let GConId i = gConId y in GConId (nf + 1 + i)
    where
      GConId nf = gConIdMax @f
  gConNum = gConNum @f + gConNum @g
  gConFixity (L1 x) = gConFixity x
  gConFixity (R1 y) = gConFixity y
  gConIsRecord (L1 x) = gConIsRecord x
  gConIsRecord (R1 y) = gConIsRecord y

instance Constructor c => GConstructors (M1 C c f) where
  gConIdToString _ = conName (M1 Proxy :: M1 C c Proxy ())
  gConId _ = GConId 0
  gConNum = 1
  gConFixity = conFixity
  gConIsRecord = conIsRecord

instance GConstructors V1 where
  gConIdToString x = x `seq` error "gConIdToString: empty type"  -- Input should be empty.
  gConId v = case v of {}
  gConNum = 0
  gConFixity v = case v of {}
  gConIsRecord v = case v of {}

-- *** Find a constructor tag by name

type ConIdNamed' n t = GConIdNamedIf n t (GConIdNamed n (Rep t))

type GConIdNamed n f = GConIdNamed' n f 0 'Nothing

type family GConIdNamed' (n :: Symbol) (f :: k -> Type) (i :: Nat) (o :: Maybe Nat) :: Maybe Nat where
  GConIdNamed' n (M1 D _c f) i r = GConIdNamed' n f i r
  GConIdNamed' n (f :+: g) i r = GConIdNamed' n f i (GConIdNamed' n g (i + NConstructors f) r)
  GConIdNamed' n (M1 C ('MetaCons n _f _s) _g) i _r = 'Just i
  GConIdNamed' n (M1 C ('MetaCons _n _f _s) _g) _i r = r
  GConIdNamed' _n V1 _i r = r

type family GConIdNamedIf (n :: Symbol) (t :: Type) (o :: Maybe Nat) :: Nat where
  GConIdNamedIf _n _t ('Just i) = i
  GConIdNamedIf  n  t 'Nothing = TypeError
    ('Text "No constructor named " ':<>: 'ShowType n
    ':<>: 'Text " in generic type " ':<>: 'ShowType t)

-- *** Check that a type is not empty

-- | Constraint that a generic type @a@ is not empty.
-- Producing an error message otherwise.
--
-- The 'Symbol' parameter @fname@ is used only for error messages.
--
-- It is implied by the simpler constraint @'IsEmptyType' a ~ 'False@
class    NonEmptyType_ fname a => NonEmptyType fname a
instance NonEmptyType_ fname a => NonEmptyType fname a

-- | Internal definition of 'NonEmptyType'.
-- It is implied by the simpler constraint @'IsEmptyType' a ~ 'False@.
--
-- >>> :set -XTypeFamilies
-- >>> :{
-- conIdMin' :: (Constructors a, IsEmptyType a ~ 'False) => ConId a
-- conIdMin' = conIdMin
-- :}
--
-- >>> :{
-- conIdMax' :: (Constructors a, IsEmptyType a ~ 'False) => ConId a
-- conIdMax' = conIdMax
-- :}
type NonEmptyType_ fname a = (ErrorIfEmpty fname a (IsEmptyType a) ~ '())

-- 'True' if the generic representation is @M1 D _ V1@.
type family GIsEmptyType (r :: k -> Type) :: Bool where
  GIsEmptyType (M1 D _d V1) = 'True
  GIsEmptyType (M1 D _d (M1 C _c _f)) = 'False
  GIsEmptyType (M1 D _d (_f :+: _g)) = 'False

-- | 'True' if the generic type @a@ is empty.
type IsEmptyType a = IsEmptyType_ a

-- | Internal definition of 'IsEmptyType'.
type IsEmptyType_ a = GIsEmptyType (Rep a)

-- | Throw an error if the boolean @b@ is true, meaning that the type @a@ is empty.
--
-- Example:
--
-- > ghci> data E deriving Generic
-- > ghci> conIdMin :: ConId E
--
-- Error message:
--
-- > The function 'conIdMin' cannot be used with the empty type E
type family ErrorIfEmpty (fname :: Symbol) (a :: Type) (b :: Bool) :: () where
  ErrorIfEmpty fname a 'True = TypeError
    ('Text "The function '" ':<>: 'Text fname
    ':<>: 'Text "' cannot be used with the empty type " ':<>: 'ShowType a)
  ErrorIfEmpty fname a 'False = '()

-- * Type families

-- | 'Meta' field of the 'M1' type constructor.
type family MetaOf (f :: Type -> Type) :: Meta where
  MetaOf (M1 i d f) = d

-- Variable names borrowed from the documentation on 'Meta'.

-- | Name of the data type ('MetaData').
type family MetaDataName (m :: Meta) :: Symbol where
  MetaDataName ('MetaData n _m _p _nt) = n

-- | Name of the module where the data type is defined ('MetaData')
type family MetaDataModule (m :: Meta) :: Symbol where
  MetaDataModule ('MetaData _n m _p _nt) = m

-- | Name of the package where the data type is defined ('MetaData')
type family MetaDataPackage (m :: Meta) :: Symbol where
  MetaDataPackage ('MetaData _n _m p _nt) = p

-- | @True@ if the data type is a newtype ('MetaData').
type family MetaDataNewtype (m :: Meta) :: Bool where
  MetaDataNewtype ('MetaData _n _m _p nt) = nt

-- | Name of the constructor ('MetaCons').
type family MetaConsName (m :: Meta) :: Symbol where
  MetaConsName ('MetaCons n _f _s) = n

-- | Fixity of the constructor ('MetaCons').
type family MetaConsFixity (m :: Meta) :: FixityI where
  MetaConsFixity ('MetaCons _n f s) = f

-- | @True@ for a record constructor ('MetaCons').
type family MetaConsRecord (m :: Meta) :: Bool where
  MetaConsRecord ('MetaCons _n _f s) = s

-- | @Just@ the name of the record field, if it is one ('MetaSel').
type family MetaSelNameM (m :: Meta) :: Maybe Symbol where
  MetaSelNameM ('MetaSel mn _su _ss _ds) = mn

-- | Name of the record field; undefined for non-record fields ('MetaSel').
type family MetaSelName (m :: Meta) :: Symbol where
  MetaSelName ('MetaSel ('Just n) _su _ss _ds) = n

-- | Unpackedness annotation of a field ('MetaSel').
type family MetaSelUnpack (m :: Meta) :: SourceUnpackedness where
  MetaSelUnpack ('MetaSel _mn su _ss _ds) = su

-- | Strictness annotation of a field ('MetaSel').
type family MetaSelSourceStrictness (m :: Meta) :: SourceStrictness where
  MetaSelSourceStrictness ('MetaSel _mn _su ss _ds) = ss

-- | Inferred strictness of a field ('MetaSel').
type family MetaSelStrictness (m :: Meta) :: DecidedStrictness where
  MetaSelStrictness ('MetaSel _mn _su _ss ds) = ds

-- | A placeholder for 'Meta' values.
type DummyMeta = 'MetaData "" "" "" 'False

-- | Remove an 'M1' type constructor.
type family   UnM1 (f :: k -> Type) :: k -> Type
type instance UnM1 (M1 i c f) = f
