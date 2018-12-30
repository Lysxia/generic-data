{-# LANGUAGE
    DeriveGeneric #-}

import GHC.Generics -- We need to import the constructors for Coercible to resolve
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data
import Generic.Data.Microsurgery

-- From https://stackoverflow.com/questions/53864911/derive-positional-show

newtype T = T { _unT :: Int } deriving Generic

instance Show T where
  showsPrec n = gshowsPrec n . derecordify . toData

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "microsurgery"
  [ testCase "Show" $ "T 3" @?= show (T 3)
  ]
