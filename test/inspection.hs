{-# OPTIONS_GHC -dsuppress-all #-}
{-# LANGUAGE
    BangPatterns,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    DeriveGeneric,
    DerivingVia,
    EmptyCase,
    EmptyDataDeriving,
    TemplateHaskell
    #-}

import Control.Applicative (liftA2)
import Data.Coerce (coerce)
import GHC.Generics (Generic, Generic1)
import Data.Semigroup (Sum(..), All(..))

import Test.Inspection

import Generic.Data
import Generic.Data.Microsurgery
  ( ProductSurgery
  , CopyRep
  , Surgery'(..)
  )

import Inspection.Boilerplate

-- Test cases

data T = T Int Bool
  deriving Generic
  deriving (Semigroup, Monoid)
    via ProductSurgery (CopyRep (Sum Int, All)) T
  deriving (Eq, Ord)
    via Generically T

mappendT, mappendTG :: T -> T -> T
mappendT (T a1 b1) (T a2 b2) = T (a1 + a2) (b1 && b2)
mappendTG x y = x <> y

memptyT, memptyTG :: T
memptyT = T 0 True
memptyTG = mempty

eqT, eqTG :: T -> T -> Bool
eqT (T a1 b1) (T a2 b2) = a1 == a2 && b1 == b2
eqTG = (==)

compareT, compareTG :: T -> T -> Ordering
compareT (T a1 b1) (T a2 b2) = compare a1 a2 <> compare b1 b2
compareTG = compare

inspect $ 'mappendT ==- 'mappendTG
inspect $ 'memptyT ==- 'memptyTG
inspect $ 'eqT ==- 'eqTG
inspect $ 'compareT ==- 'compareTG

data Empty a
  deriving (Generic, Generic1, Eq, Ord, Functor, Foldable, Traversable)

-- Arity 0 (nullary)
data Ary0 a = Ary0
  deriving (Generic, Generic1, Eq, Ord, Functor, Foldable, Traversable)

-- Arity 1 (unary) (Lazy, Strict, Newtype)
data Ary1 a = Ary1 a
  deriving (Generic, Generic1, Eq, Ord, Functor, Foldable, Traversable)

data Ary1' a = Ary1' !a
  deriving (Generic, Generic1, Eq, Ord, Functor, Foldable, Traversable)

newtype Ary1NT a = Ary1NT a
  deriving (Generic, Generic1, Eq, Ord, Functor, Foldable, Traversable)

-- Arity 2 (binary)
data Ary2 a = Ary2 a a
  deriving (Generic, Generic1, Eq, Ord, Functor, Foldable, Traversable)

-- Arity 4 (quaternary)
data Ary4 a = Ary4 a a [Int] [a]
  deriving (Generic, Generic1, Eq, Ord, Functor, Foldable, Traversable)

-- A big sum of stuff
data Big a
  = Big0
  | Big1 a
  | Big2 a a
  | Big4 a a a a
  | Big8 Int a [a] [Int] [a] a a a
  deriving (Generic1, Eq, Ord, Functor, Foldable, Traversable)

-- Empty

-- Stock deriving of fmap does not use an EmptyCase.
fmapEmptyRS :: (a -> b) -> Empty a -> Empty b
fmapEmptyRS _ = coerce

foldMapEmptyRS :: Monoid m => (a -> m) -> Empty a -> m
foldMapEmptyRS _ _ = mempty

--

mk_eq' ''Empty [| \ _ _ -> True |]
inspect $ 'eqEmptyR ==- 'eqEmptyS
inspect $ 'eqEmptyR ==- 'eqEmptyG

mk_compare' ''Empty [| \ _ _ -> EQ |]
inspect $ 'compareEmptyR ==- 'compareEmptyS
inspect $ 'compareEmptyR ==- 'compareEmptyG

mk_fmap ''Empty [| \ _ v -> case v of {} |]
inspect $ 'fmapEmptyRS ==- 'fmapEmptyS
inspect $ 'fmapEmptyR ==- 'fmapEmptyG

mk_foldMap ''Empty [| \ _ v -> case v of {} |]
inspect $ 'foldMapEmptyRS ==- 'foldMapEmptyS
inspect $ 'foldMapEmptyR ==- 'foldMapEmptyG

-- No EmptyCase!
mk_foldr ''Empty [| \_ b _ -> b |]
inspect $ 'foldrEmptyR ==- 'foldrEmptyS
inspect $ 'foldrEmptyR ==- 'foldrEmptyG

mk_traverse ''Empty [| \ _ v -> case v of {} |]
inspect $ 'traverseEmptyS ==- 'traverseEmptyS
inspect $ 'traverseEmptyR ==- 'traverseEmptyG

mk_sequenceA ''Empty [| \ v -> case v of {} |]
inspect $ 'sequenceAEmptyS ==- 'sequenceAEmptyS
inspect $ 'sequenceAEmptyR ==- 'sequenceAEmptyG

-- Ary0

eqAry0RS :: Ary0 a -> Ary0 a -> Bool
eqAry0RS Ary0 Ary0 = True

compareAry0RS :: Ary0 a -> Ary0 a -> Ordering
compareAry0RS Ary0 Ary0 = EQ

fmapAry0RS :: (a -> b) -> Ary0 a -> Ary0 b
fmapAry0RS _ = coerce

mk_eq' ''Ary0 [| \ _ _ -> True |]
inspect $ 'eqAry0RS ==- 'eqAry0S
inspect $ 'eqAry0R ==- 'eqAry0G

mk_compare' ''Ary0 [| \ _ _ -> EQ |]
inspect $ 'compareAry0RS ==- 'compareAry0S
inspect $ 'compareAry0R ==- 'compareAry0G

mk_fmap ''Ary0 [| \ _ _ -> Ary0 |]
inspect $ 'fmapAry0RS ==- 'fmapAry0S
inspect $ 'fmapAry0R ==- 'fmapAry0G

mk_foldMap ''Ary0 [| \ _ _ -> mempty |]
inspect $ 'foldMapAry0R ==- 'foldMapAry0S
inspect $ 'foldMapAry0R ==- 'foldMapAry0G

mk_foldr ''Ary0 [| \_ b _ -> b |]
inspect $ 'foldrAry0R ==- 'foldrAry0S
inspect $ 'foldrAry0R ==- 'foldrAry0G

mk_traverse ''Ary0 [| \ _ _ -> pure Ary0 |]
inspect $ 'traverseAry0S ==- 'traverseAry0S
inspect $ 'traverseAry0R ==- 'traverseAry0G

mk_sequenceA ''Ary0 [| \ _ -> pure Ary0 |]
inspect $ 'sequenceAAry0S ==- 'sequenceAAry0S
inspect $ 'sequenceAAry0R ==- 'sequenceAAry0G

-- Ary1

eqAry1RS :: Eq a => Ary1 a -> Ary1 a -> Bool
eqAry1RS (Ary1 x1) (Ary1 y1) = x1 == y1

compareAry1RS :: Ord a => Ary1 a -> Ary1 a -> Ordering
compareAry1RS (Ary1 x1) (Ary1 y1) = compare x1 y1

fmapAry1RS :: (a -> b) -> Ary1 a -> Ary1 b
fmapAry1RS f (Ary1 x) = Ary1 (f x)

foldMapAry1RS :: Monoid m => (a -> m) -> Ary1 a -> m
foldMapAry1RS f (Ary1 x) = f x

foldrAry1RS :: (a -> b -> b) -> b -> Ary1 a -> b
foldrAry1RS f b (Ary1 x) = f x b

traverseAry1RS :: Applicative f => (a -> f b) -> Ary1 a -> f (Ary1 b)
traverseAry1RS f (Ary1 x) = Ary1 <$> f x

sequenceAAry1RS :: Applicative f => Ary1 (f a) -> f (Ary1 a)
sequenceAAry1RS (Ary1 x) = Ary1 <$> x

mk_eq ''Ary1 [| \ ~(Ary1 x1) ~(Ary1 y1) -> x1 == y1 |]
inspect $ 'eqAry1RS ==- 'eqAry1S
inspect $ 'eqAry1R ==- 'eqAry1G

mk_compare ''Ary1 [| \ ~(Ary1 x1) ~(Ary1 y1) -> compare x1 y1 |]
inspect $ 'compareAry1RS ==- 'compareAry1S
inspect $ 'compareAry1R ==- 'compareAry1G

mk_fmap ''Ary1 [| \ f ~(Ary1 x) -> Ary1 (f x) |]
inspect $ 'fmapAry1RS ==- 'fmapAry1S
inspect $ 'fmapAry1R ==- 'fmapAry1G

mk_foldMap ''Ary1 [| \ f ~(Ary1 x) -> f x |]
inspect $ 'foldMapAry1RS ==- 'foldMapAry1S
inspect $ 'foldMapAry1R ==- 'foldMapAry1G

mk_foldr ''Ary1 [| \ f r ~(Ary1 x) -> f x r |]
inspect $ 'foldrAry1RS ==- 'foldrAry1S
inspect $ 'foldrAry1R ==- 'foldrAry1G

mk_traverse ''Ary1 [| \ f ~(Ary1 x) -> Ary1 <$> f x |]
inspect $ 'traverseAry1RS ==- 'traverseAry1S
inspect $ 'traverseAry1R ==- 'traverseAry1G

mk_sequenceA ''Ary1 [| \ ~(Ary1 x) -> Ary1 <$> x |]
inspect $ 'sequenceAAry1RS ==- 'sequenceAAry1S
inspect $ 'sequenceAAry1R ==- 'sequenceAAry1G

-- Generic @to@ seems to be lazy here
mk_ap ''Ary1 [| \ ~(Ary1 f1) ~(Ary1 x1) -> Ary1 (f1 x1) |]
inspect $ 'apAry1R ==- 'apAry1G

mk_liftA2 ''Ary1 [| \ f ~(Ary1 x1) ~(Ary1 x2) -> Ary1 (f x1 x2) |]
inspect $ 'liftA2Ary1R ==- 'liftA2Ary1G

-- Ary1' (strict, this is entirely the same as Ary1)

eqAry1'RS :: Eq a => Ary1' a -> Ary1' a -> Bool
eqAry1'RS (Ary1' x1) (Ary1' y1) = x1 == y1

compareAry1'RS :: Ord a => Ary1' a -> Ary1' a -> Ordering
compareAry1'RS (Ary1' x1) (Ary1' y1) = compare x1 y1

fmapAry1'RS :: (a -> b) -> Ary1' a -> Ary1' b
fmapAry1'RS f (Ary1' x) = Ary1' (f x)

foldMapAry1'RS :: Monoid m => (a -> m) -> Ary1' a -> m
foldMapAry1'RS f (Ary1' x) = f x

foldrAry1'RS :: (a -> b -> b) -> b -> Ary1' a -> b
foldrAry1'RS f b (Ary1' x) = f x b

traverseAry1'RS :: Applicative f => (a -> f b) -> Ary1' a -> f (Ary1' b)
traverseAry1'RS f (Ary1' x) = Ary1' <$> f x

sequenceAAry1'RS :: Applicative f => Ary1' (f a) -> f (Ary1' a)
sequenceAAry1'RS (Ary1' x) = Ary1' <$> x

mk_eq ''Ary1' [| \ ~(Ary1' x1) ~(Ary1' y1) -> x1 == y1 |]
inspect $ 'eqAry1'RS ==- 'eqAry1'S
inspect $ 'eqAry1'R ==- 'eqAry1'G

mk_compare ''Ary1' [| \ ~(Ary1' x1) ~(Ary1' y1) -> compare x1 y1 |]
inspect $ 'compareAry1'RS ==- 'compareAry1'S
inspect $ 'compareAry1'R ==- 'compareAry1'G

mk_fmap ''Ary1' [| \ f ~(Ary1' x) -> Ary1' (f x) |]
inspect $ 'fmapAry1'RS ==- 'fmapAry1'S
inspect $ 'fmapAry1'R ==- 'fmapAry1'G

mk_foldMap ''Ary1' [| \ f ~(Ary1' x) -> f x |]
inspect $ 'foldMapAry1'RS ==- 'foldMapAry1'S
inspect $ 'foldMapAry1'R ==- 'foldMapAry1'G

mk_foldr ''Ary1' [| \ f r ~(Ary1' x) -> f x r |]
inspect $ 'foldrAry1'RS ==- 'foldrAry1'S
inspect $ 'foldrAry1'R ==- 'foldrAry1'G

mk_traverse ''Ary1' [| \ f ~(Ary1' x) -> Ary1' <$> f x |]
inspect $ 'traverseAry1'RS ==- 'traverseAry1'S
inspect $ 'traverseAry1'R ==- 'traverseAry1'G

mk_sequenceA ''Ary1' [| \ ~(Ary1' x) -> Ary1' <$> x |]
inspect $ 'sequenceAAry1'RS ==- 'sequenceAAry1'S
inspect $ 'sequenceAAry1'R ==- 'sequenceAAry1'G

-- Generic @to@ seems to be lazy here
mk_ap ''Ary1' [| \ ~(Ary1' f1) ~(Ary1' x1) -> Ary1' (f1 x1) |]
inspect $ 'apAry1'R ==- 'apAry1'G

mk_liftA2 ''Ary1' [| \ f ~(Ary1' x1) ~(Ary1' x2) -> Ary1' (f x1 x2) |]
inspect $ 'liftA2Ary1'R ==- 'liftA2Ary1'G

-- Ary1NT

eqAry1NTRS :: Eq a => Ary1NT a -> Ary1NT a -> Bool
eqAry1NTRS = (coerce :: (a -> a -> Bool) -> Ary1NT a -> Ary1NT a -> Bool) (==)

compareAry1NTRS :: Ord a => Ary1NT a -> Ary1NT a -> Ordering
compareAry1NTRS = (coerce :: (a -> a -> Ordering) -> Ary1NT a -> Ary1NT a -> Ordering) compare

mk_eq ''Ary1NT [| \ (Ary1NT x1) (Ary1NT y1) -> x1 == y1 |]
inspect $ 'eqAry1NTRS ==- 'eqAry1NTS
inspect $ 'eqAry1NTR ==- 'eqAry1NTG

mk_compare ''Ary1NT [| \ (Ary1NT x1) (Ary1NT y1) -> compare x1 y1 |]
inspect $ 'compareAry1NTRS ==- 'compareAry1NTS
inspect $ 'compareAry1NTR ==- 'compareAry1NTG

mk_fmap ''Ary1NT [| \ f (Ary1NT x) -> Ary1NT (f x) |]
inspect $ 'fmapAry1NTR ==- 'fmapAry1NTS
inspect $ 'fmapAry1NTR ==- 'fmapAry1NTG

mk_foldMap ''Ary1NT [| \ f (Ary1NT x) -> f x |]
inspect $ 'foldMapAry1NTR ==- 'foldMapAry1NTS
inspect $ 'foldMapAry1NTR ==- 'foldMapAry1NTG

mk_foldr ''Ary1NT [| \ f r (Ary1NT x) -> f x r |]
inspect $ 'foldrAry1NTR ==- 'foldrAry1NTS
inspect $ 'foldrAry1NTR ==- 'foldrAry1NTG

mk_traverse ''Ary1NT [| \ f (Ary1NT x) -> fmap Ary1NT (f x) |]
inspect $ 'traverseAry1NTR ==- 'traverseAry1NTS
inspect $ 'traverseAry1NTR ==- 'traverseAry1NTG

mk_ap ''Ary1NT [| \ (Ary1NT f1) (Ary1NT x1) -> Ary1NT (f1 x1) |]
inspect $ 'apAry1NTR ==- 'apAry1NTG

mk_liftA2 ''Ary1NT [| \ f (Ary1NT x1) (Ary1NT x2) -> Ary1NT (f x1 x2) |]
inspect $ 'liftA2Ary1NTR ==- 'liftA2Ary1NTG

-- Ary2

mk_eq ''Ary2 [| \ (Ary2 x1 x2) (Ary2 y1 y2) -> x1 == y1 && x2 == y2 |]
inspect $ 'eqAry2R ==- 'eqAry2S
inspect $ 'eqAry2R ==- 'eqAry2G

mk_compare ''Ary2 [| \ (Ary2 x1 x2) (Ary2 y1 y2) -> compare x1 y1 <> compare x2 y2 |]
inspect $ 'compareAry2R ==- 'compareAry2S
inspect $ 'compareAry2R ==- 'compareAry2G

mk_fmap ''Ary2 [| \ f (Ary2 x y) -> Ary2 (f x) (f y) |]
inspect $ 'fmapAry2R ==- 'fmapAry2S
inspect $ 'fmapAry2R ==- 'fmapAry2G

mk_foldMap ''Ary2 [| \ f (Ary2 x y) -> f x `mappend` f y |]
inspect $ 'foldMapAry2R ==- 'foldMapAry2S
inspect $ 'foldMapAry2R ==- 'foldMapAry2G

mk_foldr ''Ary2 [| \ f r (Ary2 x y) -> f x (f y r) |]
inspect $ 'foldrAry2R ==- 'foldrAry2S
inspect $ 'foldrAry2R ==- 'foldrAry2G

mk_traverse ''Ary2 [| \ f (Ary2 x y) -> liftA2 Ary2 (f x) (f y) |]
inspect $ 'traverseAry2R ==- 'traverseAry2S
inspect $ 'traverseAry2R ==- 'traverseAry2G

mk_sequenceA ''Ary2 [| \ (Ary2 x y) -> liftA2 Ary2 x y |]
inspect $ 'sequenceAAry2R ==- 'sequenceAAry2S
inspect $ 'sequenceAAry2R ==- 'sequenceAAry2G

mk_ap ''Ary2 [| \ (Ary2 f1 f2) (Ary2 x1 x2) -> Ary2 (f1 x1) (f2 x2) |]
inspect $ 'apAry2R ==- 'apAry2G

mk_liftA2 ''Ary2 [| \ f (Ary2 x1 y1) (Ary2 x2 y2) -> Ary2 (f x1 x2) (f y1 y2) |]
inspect $ 'liftA2Ary2R ==- 'liftA2Ary2G

-- Ary4

sequenceAAry4RS :: Applicative f => Ary4 (f a) -> f (Ary4 a)
sequenceAAry4RS = traverse id

-- The simplifier is good enough to reassociate (&&)
mk_eq ''Ary4
  [| \ (Ary4 x1 x2 x3 x4) (Ary4 y1 y2 y3 y4) ->
       x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 |]
inspect $ 'eqAry4R ==- 'eqAry4S
inspect $ 'eqAry4R ==- 'eqAry4G

-- The simplifier is good enough to reassociate (<>)
mk_compare ''Ary4
  [| \ (Ary4 x1 x2 x3 x4) (Ary4 y1 y2 y3 y4) ->
       compare x1 y1 <> compare x2 y2 <> compare x3 y3 <> compare x4 y4 |]
inspect $ 'compareAry4R ==- 'compareAry4S
inspect $ 'compareAry4R ==- 'compareAry4G

mk_fmap ''Ary4
  [| \ f (Ary4 x y z t) -> Ary4 (f x) (f y) z (fmap f t) |]
inspect $ 'fmapAry4R ==- 'fmapAry4S
inspect $ 'fmapAry4R ==- 'fmapAry4G

mk_foldMap ''Ary4
  [| \ f (Ary4 x y _ z) -> f x `mappend` (f y `mappend` foldMap f z) |]
inspect $ 'foldMapAry4R ==- 'foldMapAry4S
inspect $ 'foldMapAry4R ==- 'foldMapAry4G

mk_foldr ''Ary4
  [| \ f r (Ary4 x y _ t) -> f x (f y (foldr f r t)) |]
inspect $ 'foldrAry4R ==- 'foldrAry4S
inspect $ 'foldrAry4R ==- 'foldrAry4G

mk_traverse ''Ary4
  [| \ f (Ary4 x y z t) ->
       liftA2 (\x' y' -> Ary4 x' y' z) (f x) (f y) <*> traverse f t |]
inspect $ 'traverseAry4R ==- 'traverseAry4S
inspect $ 'traverseAry4R ==- 'traverseAry4G

mk_sequenceA ''Ary4 [| \ (Ary4 x y z t) -> liftA2 (\x' y' -> Ary4 x' y' z) x y <*> sequenceA t |]
inspect $ 'sequenceAAry4RS ==- 'sequenceAAry4S
inspect $ 'sequenceAAry4R ==- 'sequenceAAry4G

mk_ap ''Ary4
  [| \ (Ary4 f1 f2 fz f3) (Ary4 x1 x2 xz x3) ->
       Ary4 (f1 x1) (f2 x2) (fz <> xz) (f3 <*> x3) |]
inspect $ 'apAry4R ==- 'apAry4G

mk_liftA2 ''Ary4
  [| \ f (Ary4 x1 y1 fz z1) (Ary4 x2 y2 xz z2) ->
       Ary4 (f x1 x2) (f y1 y2) (fz <> xz) (liftA2 f z1 z2) |]
inspect $ 'liftA2Ary4R ==- 'liftA2Ary4G

-- dummy
main :: IO ()
main = pure ()
