{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    TypeApplications,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Test.Tasty
import Test.Tasty.HUnit

import Data.Generics.Product (field_)

import Generic.Data (Generic, gshowsPrec, Opaque(Opaque))
import Generic.Data.Microsurgery (onData, toData)

data T = R { f :: Int -> Int } deriving Generic

instance Show T where 
  showsPrec n = gshowsPrec n
    . onData (field_ @"f" %~ Opaque)
    . toData

(%~) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
(%~) = coerce

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "lens-surgery"
  [ testCase "update" $
      "R {f = _}" @?= show (R id)
  ]
