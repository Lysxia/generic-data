{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Orphan instances.
--
-- The orphan instances in this module have been upstreamed in base 4.21 (GHC 9.12).
-- This module is empty starting from that version. It remains for backwards compatiblity.

module Generic.Data.Orphans where

#if __GLASGOW_HASKELL__ < 912
import Data.Functor.Classes
import Data.Orphans ()
import Data.Semigroup
import GHC.Generics

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
#endif
