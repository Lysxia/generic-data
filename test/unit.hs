{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Data.Semigroup
import Data.Monoid (Sum(..))
import Data.Functor.Classes
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Generics
import Generic.Data
import Generic.Data.Orphans ()

data P a = P a a
  deriving (Generic, Generic1)

instance Semigroup a => Semigroup (P a) where
  x <> y = unGenerically (Generically x <> Generically y)

type PTy a = a -> a -> Generically (P a)

p :: PTy a
p a b = Generically (P a b)

p' :: PTy Int
p' = p

pl :: PTy [Int]
pl = p

data P1 f a = P1 (f a) (f a)
  deriving Generic1

type PTy1 a = [a] -> [a] -> Generically1 (P1 []) a

p1 :: PTy1 a
p1 a b = Generically1 (P1 a b)

p1' :: PTy1 Int
p1' = p1

pl1 :: PTy1 [Int]
pl1 = p1

data E = E0 | E1 | E2 | E3
  deriving (Eq, Show, Generic)

data FiniteE = SE0 Bool Bool | SE1 Bool
  deriving (Eq, Ord, Show, Generic)

e0, e1, eLast :: FiniteE
e0 = allEs !! 0
e1 = allEs !! 1
eLast = last allEs

allEs :: [FiniteE]
allEs = 
    [ SE0 False False
    , SE0 False  True
    , SE0  True False
    , SE0  True  True
    , SE1 False
    , SE1 True
    ]

-- Deriving Show1
newtype MyCompose f g a = MyCompose (f (g a))
  deriving Generic1

instance (Functor f, Show1 f, Show1 g) => Show1 (MyCompose f g) where
  liftShowsPrec = gliftShowsPrec

instance (Functor f, Show1 f, Show1 g, Show a) => Show (MyCompose f g a) where
  showsPrec = showsPrec1

maybeModuleName :: String
#if MIN_VERSION_base(4,12,0)
maybeModuleName = "GHC.Maybe"
#else
maybeModuleName = "GHC.Base"
#endif

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "unit"
  [ testGroup "Eq"
      [ testCase "(==)" $ p' 1 2 @?= p' 1 2
      , testCase "(/=)" $ False @?= (p' 1 2 == p' 1 1)
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
  , testGroup "Functor"
      [ testCase "fmap" $ p1' [1] [2] @?= fmap (+ 1) (p1 [0] [1])
      ]
  , testGroup "Applicative"
      [ testCase "pure" $ p1' [3] [3] @?= pure 3
      , testCase "ap" $ p1' [1, 3] [2] @?= (p1 [id, (+2)] [(+2)] <*> p1 [1] [0])
      ]
  , testGroup "Alternative"
      [ testCase "empty" $ p1' [] [] @?= empty
      , testCase "(<|>)" $ p1' [1, 5] [2, 3] @?= (p1 [1] [2] <|> p1 [5] [3])
      ]
  , testGroup "Foldable"
      [ testCase "foldMap" $ Sum 3 @?= foldMap Sum (p1' [1] [2])
      , testCase "foldr" $ 3 @?= foldr (+) 0 (p1' [1] [2])
      ]
  , testGroup "Traversable"
      [ testCase "traverse" $
          [p1 [1] [2], p1 [1] [3], p1 [2] [2], p1 [2] [3]] @?=
            traverse (\y -> [y, y+1]) (p1' [1] [2])
      , testCase "sequenceA" $
          [p1 [1] [2], p1 [2] [2]] @?= sequenceA (pl1 [[1, 2]] [[2]])
      ]
  , testGroup "Bounded"
      [ testCase "minBound @E" $ E0 @?= gminBound
      , testCase "maxBound @E" $ E3 @?= gmaxBound
      , testCase "minBound @(P Int)" $ p' minBound minBound @?= gminBound
      , testCase "maxBound @(P Int)" $ p' maxBound maxBound @?= gmaxBound
      ]
  , testGroup "Enum"
      [ testCase "toEnum" $ [E0, E1, E2, E3] @?= fmap gtoEnum [0, 1, 2, 3]
      , testCase "fromEnum" $ [0, 1, 2, 3] @?= fmap gfromEnum [E0, E1, E2, E3]
      , testCase "enumFrom" $ [E0, E1, E2, E3] @?= genumFrom E0
      , testCase "enumFromThen" $ [E0, E1, E2, E3] @?= genumFromThen E0 E1
      , testCase "enumFromTo" $ [E0, E1, E2, E3] @?= genumFromTo E0 E3
      , testCase "enumFromThenTo" $ [E0, E1, E2, E3] @?= genumFromThenTo E0 E1 E3
      , testCase "toEnum (FiniteEnum)" $ allEs @?= fmap gtoFiniteEnum [0 .. 5]
      , testCase "fromEnum (FiniteEnum)" $ [0 .. 5] @?= fmap gfromFiniteEnum allEs
      , testCase "enumFrom (FiniteEnum)" $ allEs @?= gfiniteEnumFrom e0
      , testCase "enumFromThen (FiniteEnum)" $ allEs @?= gfiniteEnumFromThen e0 e1
      , testCase "enumFromTo (FiniteEnum)" $ allEs @?= gfiniteEnumFromTo e0 eLast
      , testCase "enumFromThenTo (FiniteEnum)" $ allEs @?= gfiniteEnumFromThenTo e0 e1 eLast
      ]
  , testGroup "Show"
      [ testCase "show" $ "P 1 2" @?= show (p' 1 2)
      , testCase "showsPrec" $ "(P 1 2)" @?= showsPrec 11 (p' 1 2) ""
      ]

  , testGroup "Show1"
      [ testCase "show1" $ "MyCompose (Just [()])" @?= show (MyCompose (Just [()]))
      ]

  , testGroup "Meta"
      [ testCase "datatypeName" $ "Maybe" @?= gdatatypeName @(Maybe Int)
      , testCase "moduleName" $ maybeModuleName @?= gmoduleName @(Maybe Int)
      , testCase "packageName" $ "base" @?= gpackageName @(Maybe Int)
      , testCase "isNewtype" $ False @?= gisNewtype @(Maybe Int)
      , testCase "conName" $ "Just" @?= gconName (Just ())
      , testCase "conFixity" $ Prefix @?= gconFixity (Just ())
      , testCase "conIsRecord" $ False @?= gconIsRecord (Just ())
      , testCase "conNum" $ 2 @?= gconNum @(Maybe Int)
      ]
  ]
