{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Data.Orphans where

import Data.Functor.Classes
import Data.Semigroup
import GHC.Generics

instance Monoid c => Applicative (K1 i c) where
  pure _ = K1 mempty
  K1 a <*> K1 b = K1 (mempty a b)

instance Semigroup (V1 p) where
  v <> _ = v

instance Semigroup (U1 p) where
  _ <> _ = U1

instance Monoid (U1 p) where
  mempty = U1
  mappend = (<>)

deriving instance Semigroup c => Semigroup (K1 i c p)
deriving instance Monoid c => Monoid (K1 i c p)

deriving instance Semigroup (f p) => Semigroup (M1 i c f p)
deriving instance Monoid (f p) => Monoid (M1 i c f p)

instance (Semigroup (f p), Semigroup (g p)) => Semigroup ((f :*: g) p) where
  (x1 :*: y1) <> (x2 :*: y2) = (x1 <> x2) :*: (y1 <> y2)

instance (Monoid (f p), Monoid (g p)) => Monoid ((f :*: g) p) where
  mempty = mempty :*: mempty
  mappend (x1 :*: y1) (x2 :*: y2) = mappend x1 x2 :*: mappend y1 y2

deriving instance Semigroup p => Semigroup (Par1 p)
deriving instance Monoid p => Monoid (Par1 p)

deriving instance Semigroup (f p) => Semigroup (Rec1 f p)
deriving instance Monoid (f p) => Monoid (Rec1 f p)

deriving instance Semigroup (f (g p)) => Semigroup ((f :.: g) p)
deriving instance Monoid (f (g p)) => Monoid ((f :.: g) p)

instance Eq1 V1 where
  liftEq _ v _ = case v of {}

instance Ord1 V1 where
  liftCompare _ v _ = case v of {}

instance Eq1 U1 where
  liftEq _ _ _ = True

instance Ord1 U1 where
  liftCompare _ _ _ = EQ

instance Eq c => Eq1 (K1 i c) where
  liftEq _ (K1 x1) (K1 x2) = x1 == x2

instance Ord c => Ord1 (K1 i c) where
  liftCompare _ (K1 x1) (K1 x2) = compare x1 x2

deriving instance Eq1 f => Eq1 (M1 i c f)
deriving instance Ord1 f => Ord1 (M1 i c f)

instance (Eq1 f, Eq1 g) => Eq1 (f :*: g) where
  liftEq (==.) (x1 :*: y1) (x2 :*: y2) = liftEq (==.) x1 x2 && liftEq (==.) y1 y2

instance (Ord1 f, Ord1 g) => Ord1 (f :*: g) where
  liftCompare compare' (x1 :*: y1) (x2 :*: y2) =
    liftCompare compare' x1 x2 <> liftCompare compare' y1 y2

instance (Eq1 f, Eq1 g) => Eq1 (f :+: g) where
  liftEq (==.) (L1 x1) (L1 x2) = liftEq (==.) x1 x2
  liftEq (==.) (R1 y1) (R1 y2) = liftEq (==.) y1 y2
  liftEq _ _ _ = False

instance (Ord1 f, Ord1 g) => Ord1 (f :+: g) where
  liftCompare compare' (L1 x1) (L1 x2) = liftCompare compare' x1 x2
  liftCompare compare' (R1 y1) (R1 y2) = liftCompare compare' y1 y2
  liftCompare _ (L1 _) (R1 _) = LT
  liftCompare _ (R1 _) (L1 _) = GT

instance Eq1 f => Eq1 (Rec1 f) where
  liftEq (==.) (Rec1 r1) (Rec1 r2) = liftEq (==.) r1 r2

instance Ord1 f => Ord1 (Rec1 f) where
  liftCompare compare' (Rec1 r1) (Rec1 r2) = liftCompare compare' r1 r2

instance Eq1 Par1 where
  liftEq (==.) (Par1 p1) (Par1 p2) = p1 ==. p2

instance Ord1 Par1 where
  liftCompare compare' (Par1 p1) (Par1 p2) = compare' p1 p2

instance (Eq1 f, Eq1 g) => Eq1 (f :.: g) where
  liftEq (==.) (Comp1 x1) (Comp1 x2) = (liftEq . liftEq) (==.) x1 x2

instance (Ord1 f, Ord1 g) => Ord1 (f :.: g) where
  liftCompare compare' (Comp1 x1) (Comp1 x2) =
    (liftCompare . liftCompare) compare' x1 x2
