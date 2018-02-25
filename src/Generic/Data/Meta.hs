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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Data.Meta where

import Data.Proxy
import GHC.Generics

-- | Name of the data constructor in a type as a string.
--
-- @
-- 'typeName' @(Maybe AnyType) = \"Maybe\"
-- @
typeName :: forall a. TypeName a => String
typeName = gTypeName @(Rep a)

class (Generic a, GTypeName (Rep a)) => TypeName a
instance (Generic a, GTypeName (Rep a)) => TypeName a

class GTypeName r where
  gTypeName :: String

instance Datatype d => GTypeName (M1 D d f) where
  gTypeName = datatypeName (M1 Proxy :: M1 D d Proxy ())

newtype ConId a = ConId Int
  deriving (Eq, Ord)

conIdToInt :: forall a. ConId a -> Int
conIdToInt (ConId i) = i

conIdEnum :: forall a. Constructors a => [ConId a]
conIdEnum = fmap ConId [0 .. n]
  where
    ConId n = conIdMax @a

constructor :: forall a. Constructors a => a -> String
constructor = conIdToString . conId

conIdToString :: forall a. Constructors a => ConId a -> String
conIdToString = gConIdToString . fromConId

conId :: forall a. Constructors a => a -> ConId a
conId = toConId . gConId . from

conIdMax :: forall a. Constructors a => ConId a
conIdMax = toConId gConIdMax

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

class GConstructors r where
  gConIdToString :: GConId r -> String
  gConId :: r p -> GConId r
  gConIdMax :: GConId r

instance GConstructors f => GConstructors (M1 D c f) where
  gConIdToString = gConIdToString @f . reGConId
  gConId (M1 x) = reGConId (gConId x)
  gConIdMax = reGConId (gConIdMax @f)

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
  gConIdMax = gConIdMax +. gConIdMax
    where
      (+.) :: GConId f -> GConId g -> GConId (f :+: g)
      GConId nf +. GConId ng = GConId (nf + ng + 1)

instance Constructor c => GConstructors (M1 C c f) where
  gConIdToString _ = conName (M1 Proxy :: M1 C c Proxy ())
  gConId _ = GConId 0
  gConIdMax = GConId 0
