-- | Type metadata accessors
--
-- Type names, constructor names...

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Data.Internal.Meta where

import Data.Proxy
import GHC.Generics

-- | Name of the first data constructor in a type as a string.
--
-- @
-- 'gdatatypeName' @('Maybe' AnyType) = \"Maybe\"
-- @
gdatatypeName :: forall a. (Generic a, GDatatype (Rep a)) => String
gdatatypeName = gDatatypeName @(Rep a)

-- | Name of the module where the first type constructor is defined.
--
-- @
-- 'gmoduleName' @('Maybe' AnyType) = \"GHC.Base\"
-- @
gmoduleName :: forall a. (Generic a, GDatatype (Rep a)) => String
gmoduleName = gModuleName @(Rep a)

-- | Name of the package where the first type constructor is defined.
--
-- @
-- 'gpackageName' @('Maybe' AnyType) = \"base\"
-- @
gpackageName :: forall a. (Generic a, GDatatype (Rep a)) => String
gpackageName = gPackageName @(Rep a)

-- | 'True' if the first type constructor is a newtype.
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
-- @
-- 'gconName' ('Just' 0) = \"Just\"
-- @
gconName :: forall a. Constructors a => a -> String
gconName = conIdToString . conId

-- | The fixity of the first constructor.
--
-- @
-- 'gconFixity' ('Just' 0) = 'Prefix'
-- 'gconFixity' ([] :*: id) = 'Infix' 'RightAssociative' 6
-- @
gconFixity :: forall a. Constructors a => a -> Fixity
gconFixity = gConFixity . from

-- | 'True' if the constructor is a record.
--
-- @
-- 'gconIsRecord' ('Just' 0) = 'False'
-- 'gconIsRecord' ('Data.Monoid.Sum' 0) = 'True'
-- -- newtype 'Data.Monoid.Sum' a = Sum { getSum :: a }
-- @
gconIsRecord :: forall a. Constructors a => a -> Bool
gconIsRecord = gConIsRecord . from

-- | Number of constructors.
--
-- @
-- 'gconNum' @('Maybe' AnyType) = 2
-- @
gconNum :: forall a. Constructors a => Int
gconNum = gConNum @(Rep a)

-- | An opaque identifier for a constructor.
newtype ConId a = ConId Int
  deriving (Eq, Ord)

conIdToInt :: forall a. ConId a -> Int
conIdToInt (ConId i) = i

conIdEnum :: forall a. Constructors a => [ConId a]
conIdEnum = fmap ConId [0 .. n]
  where
    ConId n = conIdMax @a

conIdToString :: forall a. Constructors a => ConId a -> String
conIdToString = gConIdToString . fromConId

conId :: forall a. Constructors a => a -> ConId a
conId = toConId . gConId . from

conIdMax :: forall a. Constructors a => ConId a
conIdMax = toConId gConIdMax

-- | Constraint synonym for 'Generic' and 'GConstructor'.
class (Generic a, GConstructors (Rep a)) => Constructors a
instance (Generic a, GConstructors (Rep a)) => Constructors a

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
      gConIdToString @g (GConId (i - nf - 1))
    where
      GConId nf = gConIdMax @f
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
