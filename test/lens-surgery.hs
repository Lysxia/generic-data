{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    TypeApplications,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Generics.Product (field)
import Data.Generics.Internal.VL.Lens

import Generic.Data.Surgery (onData, toData)

data T a = R { f :: a } deriving (Generic, Show)

main :: IO ()
main = defaultMain test

show' :: Show (f ()) => f () -> String
show' = show

test :: TestTree
test = testGroup "lens-surgery"
  [ testCase "update" $
      "R {f = 42}" @?= (show' . onData (field @"f" .~ (42 :: Int)) . toData) (R ())
  ]
