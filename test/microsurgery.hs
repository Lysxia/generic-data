{-# LANGUAGE
    DeriveGeneric,
    DataKinds,
    TypeApplications #-}

-- @DataKinds@ and @TypeApplications@ for @renameFields@ and @renameConstrs@

import GHC.Generics -- We need to import the constructors for Coercible to resolve
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data
import Generic.Data.Microsurgery

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

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "microsurgery"
  [ testCase "Show T" $ "T 3" @?= show (T 3)
  , testCase "Show U" $ "V {unV = 3}" @?= show (U 3)
  ]
