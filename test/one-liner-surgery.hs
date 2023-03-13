{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    FlexibleInstances,
    LambdaCase,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- Example using one-liner and generic-lens
-- on a synthetic type obtained by surgery.

import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Generics.Product (field_)  -- generic-lens
import Generics.OneLiner (nullaryOp, binaryOp)  -- one-liner
import Generics.OneLiner.Binary (gtraverse)

import Generic.Data.Microsurgery (DOnFields)

-- | Toy configuration record type.
data Config = C {
    a :: Int,
    b :: Int,
    c :: String
  } deriving (Eq, Generic, Show)

-- | Applying the 'DOnFields' surgery to get a type isomorphic to:
--
-- > data Config = C {
-- >     a :: Maybe Int,
-- >     b :: Maybe Int,
-- >     c :: Maybe String
-- >   }
--
-- See also "Functor functors" and "Higher-kinded data" for a more general pattern:
--
-- - https://www.benjamin.pizza/posts/2017-12-15-functor-functors.html
-- - https://reasonablypolymorphic.com/blog/higher-kinded-data/
--
type PartialConfig = DOnFields Maybe Config

-- | Example
file1 :: [String]
file1 = [
    "a=11",
    "b=33"
  ]

-- | Example
file2 :: [String]
file2 = [
    "b=2",
    "c=Hello"
  ]

-- | Helper for 'emptyOM' and 'mergeOM' below.
class    (a ~ Maybe (UnMaybe a)) => IsMaybe a
instance (a ~ Maybe (UnMaybe a)) => IsMaybe a

-- | Helper for 'IsMaybe' above.
type family UnMaybe (a :: Type) :: Type where
  UnMaybe (Maybe b) = b

-- |
-- > emptyOM = C {
-- >     a = Nothing,
-- >     b = Nothing,
-- >     c = Nothing
-- >   }
emptyOM :: PartialConfig
emptyOM = nullaryOp @IsMaybe Nothing

-- | Helper for 'parseOM' (actually a function from lens).
--
-- @(l .~ b) s@: set the field of record @s@ focused by lens @l@ to @b@.
--
-- > let f = (field_ @"a" .~ v) in
-- > f (C {a = x, b = y, c = z})
-- >
-- > -- equals --
-- >
-- > C {a = v, b = y, c = z}
--
(.~) :: forall s t a b. ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
(.~) l b = coerce l (const b :: a -> b)

-- | Parse lines of a config file.
parseOM :: [String] -> PartialConfig
parseOM = foldr ($) emptyOM . map (\case
  'a' : '=' : n -> field_ @"a" .~ readMaybe n
  'b' : '=' : n -> field_ @"b" .~ readMaybe n
  'c' : '=' : s -> field_ @"c" .~ Just s
  _ -> id)

-- | Merge two records of 'Maybe' fields, keeping the leftmost 'Just' for each
-- field.
mergeOM :: PartialConfig -> PartialConfig -> PartialConfig
mergeOM = binaryOp @IsMaybe (<|>)

-- | Example
parsedOpts12 :: PartialConfig
parsedOpts12 = parseOM file1 `mergeOM` parseOM file2

-- | Helper for 'validateOM' below.
class    (a ~ Maybe b) => FstIsMaybe a b
instance (a ~ Maybe b) => FstIsMaybe a b

-- | Check that all fields are populated with 'Just' and create a plain
-- 'Config' record. If any field is 'Nothing', returns 'Nothing'.
validateOM :: PartialConfig -> Maybe Config
validateOM = gtraverse @FstIsMaybe id

-- | Example
opts12 :: Maybe Config
opts12 = validateOM parsedOpts12

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "one-liner-surgery"
  [ testCase "opts1" $
      "C {a = Just 11, b = Just 33, c = Nothing}" @=? show (parseOM file1)

  , testCase "opts2" $
      "C {a = Nothing, b = Just 2, c = Just \"Hello\"}" @=? show (parseOM file2)

  , testCase "opts12" $
      Just C {a = 11, b = 33, c = "Hello"} @=? opts12

  , testCase "opts1-incomplete" $
      Nothing @=? validateOM (parseOM file1)

  , testCase "empty" $
      "C {a = Nothing, b = Nothing, c = Nothing}" @=? show emptyOM
  ]
