{-# LANGUAGE
    CPP,
    DataKinds,
    DeriveGeneric,
    TypeApplications #-}

import Control.Applicative
import Data.Ix
import Data.Semigroup
import Data.Monoid (Sum(..))
import Data.Functor.Classes
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read

import GHC.Generics (Fixity(Prefix))
import Generic.Data
import Generic.Data.Orphans ()

data P a = P a a
  deriving (Generic, Generic1)

instance Semigroup a => Semigroup (P a) where
  x <> y = case Generically x <> Generically y of
    Generically z -> z

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
  deriving (Eq, Ord, Show, Generic, Ix)

data FiniteE = SE0 Bool Bool | SE1 Bool
  deriving (Eq, Ord, Show, Generic)

data TupleE = T E E
  deriving (Eq, Ord, Show, Generic)

data Unit = Unit
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

instance (Functor f, Eq1 f, Eq1 g) => Eq1 (MyCompose f g) where
  liftEq = gliftEq

instance (Functor f, Eq1 f, Eq1 g, Eq a) => Eq (MyCompose f g a) where
  (==) = eq1

instance (Functor f, Read1 f, Read1 g) => Read1 (MyCompose f g) where
#if MIN_VERSION_base(4,10,0)
  liftReadPrec = gliftReadPrec
  liftReadListPrec = liftReadListPrecDefault
#else
  liftReadsPrec rp rl = readPrec_to_S $
    gliftReadPrec (readS_to_Prec rp) (readS_to_Prec (const rl))
#endif

instance (Functor f, Read1 f, Read1 g, Read a) => Read (MyCompose f g a) where
#if MIN_VERSION_base(4,10,0)
  readPrec = readPrec1
  readListPrec = readListPrecDefault
#else
  readsPrec = readsPrec1
#endif

instance (Functor f, Show1 f, Show1 g) => Show1 (MyCompose f g) where
  liftShowsPrec = gliftShowsPrec

instance (Functor f, Show1 f, Show1 g, Show a) => Show (MyCompose f g a) where
  showsPrec = showsPrec1

-- Regression tests for T30
data T30a = MkT30a { (##) :: () }
  deriving Generic

data T30b = (:!:) () ()
          | () `MkT30b` ()
  deriving Generic

instance Eq T30a where
  (==) = geq

instance Read T30a where
  readPrec = greadPrec
  readListPrec = readListPrecDefault

instance Show T30a where
  showsPrec = gshowsPrec

instance Eq T30b where
  (==) = geq

instance Read T30b where
  readPrec = greadPrec
  readListPrec = readListPrecDefault

instance Show T30b where
  showsPrec = gshowsPrec

maybeModuleName :: String
#if MIN_VERSION_base(4,20,0)
maybeModuleName = "GHC.Internal.Maybe"
#elif MIN_VERSION_base(4,12,0)
maybeModuleName = "GHC.Maybe"
#else
maybeModuleName = "GHC.Base"
#endif

maybePackageName :: String
#if MIN_VERSION_base(4,20,0)
maybePackageName = "ghc-internal"
#else
maybePackageName = "base"
#endif

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "unit"
  [ testGroup "Eq"
      [ testCase "(==)" $ p' 1 2 @=? p' 1 2
      , testCase "(/=)" $ False @=? (p' 1 2 == p' 1 1)
      ]
  , testGroup "Ord"
      [ testCase "compare" $ LT @=? compare (p' 1 2) (p' 2 1)
      , testCase "(<=)" $ True @=? (p' 1 1 <= p' 1 1)
      ]
  , testGroup "Semigroup"
      [ testCase "(<>)" $ pl [1, 5] [2, 3] @=? (pl [1] [2] <> pl [5] [3])
      ]
  , testGroup "Monoid"
      [ testCase "mempty" $ pl [] [] @=? mempty
      ]
  , testGroup "Functor"
      [ testCase "fmap" $ p1' [1] [2] @=? fmap (+ 1) (p1 [0] [1])
      ]
  , testGroup "Applicative"
      [ testCase "pure" $ p1' [3] [3] @=? pure 3
      , testCase "ap" $ p1' [1, 3] [2] @=? (p1 [id, (+2)] [(+2)] <*> p1 [1] [0])
      ]
  , testGroup "Alternative"
      [ testCase "empty" $ p1' [] [] @=? empty
      , testCase "(<|>)" $ p1' [1, 5] [2, 3] @=? (p1 [1] [2] <|> p1 [5] [3])
      ]
  , testGroup "Foldable"
      [ testCase "foldMap" $ Sum 3 @=? foldMap Sum (p1' [1] [2])
      , testCase "foldr" $ 3 @=? foldr (+) 0 (p1' [1] [2])
      ]
  , testGroup "Traversable"
      [ testCase "traverse" $
          [p1 [1] [2], p1 [1] [3], p1 [2] [2], p1 [2] [3]] @=?
            traverse (\y -> [y, y+1]) (p1' [1] [2])
      , testCase "sequenceA" $
          [p1 [1] [2], p1 [2] [2]] @=? sequenceA (pl1 [[1, 2]] [[2]])
      ]
  , testGroup "Bounded"
      [ testCase "minBound @E" $ E0 @=? gminBound
      , testCase "maxBound @E" $ E3 @=? gmaxBound
      , testCase "minBound @(P Int)" $ p' minBound minBound @=? gminBound
      , testCase "maxBound @(P Int)" $ p' maxBound maxBound @=? gmaxBound
      ]
  , testGroup "Enum"
      [ testGroup "StandardEnum"
        [ testCase "toEnum" $ [E0, E1, E2, E3] @=? fmap gtoEnum [0, 1, 2, 3]
        , testCase "fromEnum" $ [0, 1, 2, 3] @=? fmap gfromEnum [E0, E1, E2, E3]
        , testCase "enumFrom" $ [E0, E1, E2, E3] @=? genumFrom E0
        , testCase "enumFromThen" $ [E0, E1, E2, E3] @=? genumFromThen E0 E1
        , testCase "enumFromTo" $ [E0, E1, E2, E3] @=? genumFromTo E0 E3
        , testCase "enumFromThenTo" $ [E0, E1, E2, E3] @=? genumFromThenTo E0 E1 E3
        ]
      , testGroup "FiniteEnum"
        [ testCase "toEnum" $ allEs @=? fmap gtoFiniteEnum [0 .. 5]
        , testCase "fromEnum" $ [0 .. 5] @=? fmap gfromFiniteEnum allEs
        , testCase "enumFrom" $ allEs @=? gfiniteEnumFrom e0
        , testCase "enumFromThen" $ allEs @=? gfiniteEnumFromThen e0 e1
        , testCase "enumFromTo" $ allEs @=? gfiniteEnumFromTo e0 eLast
        , testCase "enumFromThenTo" $ allEs @=? gfiniteEnumFromThenTo e0 e1 eLast
        ]
      ]
  , testGroup "Ix"
      [ testGroup "only nullary constructors"
        [ testCase "range" $ [E0, E1, E2] @=? grange (E0, E2)
        , testCase "index" $ 1 @=? gindex (E1, E3) E2
        , testCase "inRange (within)" $ True @=? ginRange (E1, E3) E2
        , testCase "inRange (outside)" $ False @=? ginRange (E1, E3) E0
        ]
      , testGroup "single constructor"
        [ testCase "range" $ [T E1 E2, T E1 E3, T E2 E2, T E2 E3]  @=?
          grange (T E1 E2, T E2 E3)
        , testCase "index" $ 2 @=? gindex (T E1 E2, T E2 E3) (T E2 E2)
        , testCase "inRange (within)" $ True @=? ginRange (T E1 E2, T E2 E3) (T E1 E3)
        , testCase "inRange (outside)" $ False @=? ginRange (T E1 E2, T E2 E3) (T E2 E1)
        ]
      , testCase "single nullary constructor" $ 0 @=? gindex (Unit, Unit) Unit
      ]
  , testGroup "Read"
      [ testCase "read" $ p' 1 2 @=? read "(P 1 2)"
      , testGroup "T30"
        [ testCase "MkT30a" $ MkT30a {(##) = ()} @=? read "(MkT30a {(##) = ()})"
        , testCase "(:!:)" $ (:!:) () () @=? read "(:!:) () ()"
        , testCase "MkT30b" $ (() `MkT30b` ()) @=? read "() `MkT30b` ()"
        ]
      ]
  , testGroup "Show"
      [ testCase "show" $ "P 1 2" @=? show (p' 1 2)
      , testCase "showsPrec" $ "(P 1 2)" @=? showsPrec 11 (p' 1 2) ""
      , testGroup "T30"
        [ testCase "MkT30a" $ "(MkT30a {(##) = ()})" @=? showsPrec 11 (MkT30a {(##) = ()}) ""
        , testCase "(:!:)" $ "(:!:) () ()" @=? show ((:!:) () ())
        , testCase "MkT30b" $ "() `MkT30b` ()" @=? show (() `MkT30b` ())
        ]
      ]

  , testGroup "Read1"
      [ testCase "read1" $ MyCompose (Just [()]) @?= read "(MyCompose (Just [()]))"
      ]
  , testGroup "Show1"
      [ testCase "show1" $ "MyCompose (Just [()])" @?= show (MyCompose (Just [()]))
      ]

  , testGroup "Meta"
      [ testCase "datatypeName" $ "Maybe" @=? gdatatypeName @(Maybe Int)
      , testCase "moduleName" $ maybeModuleName @=? gmoduleName @(Maybe Int)
      , testCase "packageName" $ maybePackageName @=? gpackageName @(Maybe Int)
      , testCase "isNewtype" $ False @=? gisNewtype @(Maybe Int)
      , testCase "conName" $ "Just" @=? gconName (Just ())
      , testCase "conFixity" $ Prefix @=? gconFixity (Just ())
      , testCase "conIsRecord" $ False @=? gconIsRecord (Just ())
      , testCase "conNum" $ 2 @=? gconNum @(Maybe Int)
      ]
  , testGroup "ConId"
      [ testCase "conIdEnum" $ [conId Nothing, conId (Just ())] @?= conIdEnum @(Maybe ())
      , testCase "conIdMin" $ conId (Nothing :: Maybe ()) @?= conIdMin
      , testCase "conIdMax" $ conId (Just ()) @?= conIdMax
      ]
  , let i = conId (Nothing :: Maybe ()) in
    testGroup "ConId (Nothing)"
      [ testCase "conId" $ "ConId 0" @?= show i
      , testCase "conIdToInt" $ 0 @?= conIdToInt i
      , testCase "conIdToString" $ "Nothing" @?= conIdToString i
      , testCase "conIdNamed" $ i @?= conIdNamed @"Nothing"
      ]
  , let i = conId (Just ()) in
    testGroup "ConId (Just)"
      [ testCase "conId" $ "ConId 1" @?= show i
      , testCase "conIdToInt" $ 1 @?= conIdToInt i
      , testCase "conIdToString" $ "Just" @?= conIdToString i
      , testCase "conIdNamed" $ i @?= conIdNamed @"Just"
      ]
  ]
