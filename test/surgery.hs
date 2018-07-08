{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic(..))
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data.Internal.Surgery

data T = A | B Int | C Int Int Int Int Int deriving (Eq, Show, Generic)

x :: T
x = C 1 2 3 4 5

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "surgery"
  [ testCase "roundtrip" $ x @?= (arborify . linearize) x
  ]
