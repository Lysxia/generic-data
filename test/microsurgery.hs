{-# LANGUAGE
    CPP,
    DeriveGeneric,
    DataKinds,
    TypeApplications #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DerivingVia #-}
#endif

-- @DataKinds@ and @TypeApplications@ for @renameFields@ and @renameConstrs@

import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data (gshowsPrec)
import Generic.Data.Microsurgery
  ( toData
  , derecordify, typeage, renameFields, renameConstrs
  , SConst, SError, SRename
  )

#if __GLASGOW_HASKELL__ >= 806
import Data.Monoid (Sum(..))

-- DerivingVia test
-- Constructors must be visible for Coercible
import Generic.Data.Microsurgery
  ( Surgery, ProductSurgery, Surgery'(..), Generically(..), GenericProduct(..)
  , Derecordify, OnFields
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
#endif

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "microsurgery"
  [ testCase "Show T" $ "T 3" @?= show (T 3)
  , testCase "Show U" $ "V {unV = 3}" @?= show (U 3)
#if __GLASGOW_HASKELL__ >= 806
  , testCase "Show V" $ "V 3 4" @?= show (V 3 4)
#endif
  ]
