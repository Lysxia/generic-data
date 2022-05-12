{-# LANGUAGE
    CPP,
    DeriveGeneric,
    DataKinds,
    TypeApplications #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE
    DerivingVia,
    ExplicitNamespaces,
    TypeOperators #-}
#endif

-- @DataKinds@ and @TypeApplications@ for @renameFields@ and @renameConstrs@

import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data (Generic, gshowsPrec)
import Generic.Data.Microsurgery
  ( toData
  , derecordify, typeage, renameFields, renameConstrs
  , SConst, SError, SRename
  )

#if __GLASGOW_HASKELL__ >= 806
-- DerivingVia test
-- Constructors must be visible for Coercible
import Data.Monoid (Sum(..), Product(..))

import Generic.Data (Opaque(..))
import Generic.Data.Microsurgery
  ( Surgery, Surgeries, ProductSurgery, ProductSurgeries, Surgery'(..), Generically(..), GenericProduct(..)
  , Derecordify, OnFields, CopyRep
  , type (%~)
  )
#endif

-- From https://stackoverflow.com/questions/53864911/derive-positional-show

newtype T = T { _unT :: Int } deriving Generic

instance Show T where
  showsPrec n = gshowsPrec n . derecordify . toData

newtype U = U { _unU :: Int } deriving Generic

instance Show U where
  showsPrec n =
    gshowsPrec n
      . renameFields @(SRename '[ '("_unU", "unV")] SError)
      . renameConstrs @(SConst "V")
      . typeage  -- doesn't change anything, just a sanity check.
      . toData

#if __GLASGOW_HASKELL__ >= 806
data V = V { v1 :: Int, v2 :: Int }
  deriving Generic
  deriving Show via (Surgery Derecordify V)
  deriving (Semigroup, Monoid) via (ProductSurgery (OnFields Sum) V)

data Polar a = Exp { modulus :: a, argument :: a }
  deriving Generic
  deriving Show via (Surgery Derecordify (Polar a))
  deriving (Semigroup, Monoid) via (ProductSurgery (CopyRep (Product a, Sum a)) (Polar a))

data Vec a = Vec
  { len :: Int
  , contents :: [a] }
  deriving Generic
  deriving (Eq, Show) via Generically (Vec a)
  deriving (Semigroup, Monoid) via ProductSurgeries '["len" %~ Data.Monoid.Sum] (Vec a)

data Unshowable = Unshowable
  { fun :: Int -> Int
  , io :: IO Bool
  , int :: Int }
  deriving Generic
  deriving Show via Surgeries '["fun" %~ Opaque, "io" %~ Opaque] Unshowable
#endif

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "microsurgery"
  [ testCase "Show T" $ "T 3" @?= show (T 3)
  , testCase "Show U" $ "V {unV = 3}" @?= show (U 3)
#if __GLASGOW_HASKELL__ >= 806
  , testCase "Show V" $ "V 3 4" @?= show (V 3 4)
  , testCase "Semigroup V" $ "V 5 6" @?= show (V 2 3 <> V 3 3)
  , testCase "Monoid Polar" $ "Exp 1 0" @?= show (mempty :: Polar Int)
  , testCase "Semigroup Polar" $ "Exp 9 6" @?= show (Exp 3 4 <> Exp 3 2 :: Polar Int)
  , testCase "Vec" $ Vec 3 [1,2,3] @?= (Vec 1 [1 :: Int] <> Vec 2 [2,3])
  , testCase "Unshowable" $ "Unshowable {fun = _, io = _, int = 42}" @?= show (Unshowable id (pure True) 42)
#endif
  ]
