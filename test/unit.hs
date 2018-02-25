{-# LANGUAGE DeriveGeneric #-}

import Data.Semigroup
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Generics
import Generic.Data
import Generic.Data.Orphans ()

data P a = P a a
  deriving Generic

type PTy a = a -> a -> Generically (P a)

p :: PTy a
p a b = Generically (P a b)

p' :: PTy Int
p' = p

pl :: PTy [Int]
pl = p

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "unit"
  [ testGroup "Eq"
      [ testCase "(==)" $ p' 1 2 @?= p' 1 2
      , testCase "(/=)" $ False @?= (p' 1 2 == p' 2 1)
      ]
  , testGroup "Ord"
      [ testCase "compare" $ LT @?= compare (p' 1 2) (p' 2 1)
      , testCase "(<=)" $ True @?= (p' 1 1 <= p' 1 1)
      ]
  , testGroup "Semigroup"
      [ testCase "(<>)" $ pl [1, 5] [2, 3] @?= (pl [1] [2] <> pl [5] [3])
      ]
  , testGroup "Monoid"
      [ testCase "mempty" $ pl [] [] @?= mempty
      ]
  ]
