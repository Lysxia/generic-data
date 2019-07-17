{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Generic.Data.Internal.Bi (gbimap , gbifoldMap , gbitraverse) where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Semigroup
import GHC.Generics

class GBifunctor s t a b c d where
  gbimap' :: (a -> b) -> (c -> d) -> s x -> t x

class (Semigroup m , Monoid m) => GBifoldable s a b m where
  gbifoldMap' :: (a -> m) -> (b -> m) -> s x -> m

class GBitraversable s t a b c d where
  gbitraverse' :: Applicative f => (a -> f b) -> (c -> f d) -> s x -> f (t x)

instance GBifunctor s t a b c d => GBifunctor (M1 k m s) (M1 k m t) a b c d where
  gbimap' f g (M1 m) = M1 (gbimap' f g m)

instance GBifoldable s a b m => GBifoldable (M1 k w s) a b m where
  gbifoldMap' f g (M1 m) = gbifoldMap' f g m

instance GBitraversable s t a b c d => GBitraversable (M1 k m s) (M1 k m t) a b c d where
  gbitraverse' f g (M1 m) = M1 <$> gbitraverse' f g m


instance (GBifunctor l l' a b c d , GBifunctor r r' a b c d) => GBifunctor (l :+: r) (l' :+: r') a b c d where
  gbimap' f g (L1 x) = L1 (gbimap' f g x)
  gbimap' f g (R1 x) = R1 (gbimap' f g x)

instance (GBifoldable l a b m , GBifoldable r a b m) => GBifoldable (l :+: r) a b m where
  gbifoldMap' f g (L1 x) = gbifoldMap' f g x
  gbifoldMap' f g (R1 x) = gbifoldMap' f g x

instance (GBitraversable l l' a b c d , GBitraversable r r' a b c d) => GBitraversable (l :+: r) (l' :+: r') a b c d where
  gbitraverse' f g (L1 x) = L1 <$> gbitraverse' f g x
  gbitraverse' f g (R1 x) = R1 <$> gbitraverse' f g x

instance (GBifunctor l l' a b c d , GBifunctor r r' a b c d) => GBifunctor (l :*: r) (l' :*: r') a b c d where
  gbimap' f g (x :*: y) = gbimap' f g x :*: gbimap' f g y

instance (GBifoldable l a b m , GBifoldable r a b m) => GBifoldable (l :*: r) a b m where
  gbifoldMap' f g (x :*: y) = gbifoldMap' f g x <> gbifoldMap' f g y

instance (GBitraversable l l' a b c d , GBitraversable r r' a b c d) => GBitraversable (l :*: r) (l' :*: r') a b c d where
  gbitraverse' f g (x :*: y) = (:*:) <$> gbitraverse' f g x <*> gbitraverse' f g y


instance GBifunctor U1 U1 a b c d where
  gbimap' _ _ = id

instance (Semigroup m , Monoid m) => GBifoldable U1 a b m where
  gbifoldMap' _ _ _ = mempty

instance GBitraversable U1 U1 a b c d where
  gbitraverse' _ _ = pure

instance GBifunctor V1 V1 a b c d where
  gbimap' _ _ _ = undefined

instance (Semigroup m , Monoid m) => GBifoldable V1 a b m where
  gbifoldMap' _ _ _ = undefined

instance GBitraversable V1 V1 a b c d where
  gbitraverse' _ _ _ = undefined

instance {-# INCOHERENT #-} GBifunctor (K1 i a) (K1 i b) a b c d where
  gbimap' f _ (K1 x) = K1 (f x)

instance {-# INCOHERENT #-} GBifunctor (K1 i c) (K1 i d) a b c d where
  gbimap' _ g (K1 x) = K1 (g x)

instance {-# INCOHERENT #-} GBifunctor (K1 i w) (K1 i w) a b c d where
  gbimap' _ _ = id

instance {-# INCOHERENT #-} (Semigroup m , Monoid m) => GBifoldable (K1 i a) a b m where
  gbifoldMap' f _ (K1 x) = f x

instance {-# INCOHERENT #-} (Semigroup m , Monoid m) => GBifoldable (K1 i b) a b m where
  gbifoldMap' _ g (K1 x) = g x

instance {-# INCOHERENT #-} (Semigroup m , Monoid m) => GBifoldable (K1 i w) a b m where
  gbifoldMap' _ _ _ = mempty

instance {-# INCOHERENT #-} GBitraversable (K1 i a) (K1 i b) a b c d where
  gbitraverse' f _ (K1 x) = K1 <$> f x

instance {-# INCOHERENT #-} GBitraversable (K1 i c) (K1 i d) a b c d where
  gbitraverse' _ g (K1 x) = K1 <$> g x

instance {-# INCOHERENT #-} GBitraversable (K1 i w) (K1 i w) a b c d where
  gbitraverse' _ _ = pure




instance {-# INCOHERENT #-} Bifunctor f
  => GBifunctor (K1 i (f a c)) (K1 i (f b d)) a b c d where
  gbimap' f g (K1 a) = K1 (bimap f g a)

instance {-# INCOHERENT #-} Functor f
  => GBifunctor (K1 i (f c)) (K1 i (f d)) a b c d where
  gbimap' _ g (K1 a) = K1 (fmap g a)

instance {-# INCOHERENT #-} Functor f
  => GBifunctor (K1 i (f a)) (K1 i (f b)) a b c d where
  gbimap' f _ (K1 a) = K1 (fmap f a)

instance {-# INCOHERENT #-} Bifunctor f
  => GBifunctor (K1 i (f a a)) (K1 i (f b b)) a b c d where
  gbimap' f _ (K1 a) = K1 (bimap f f a)

instance {-# INCOHERENT #-} Bifunctor f
  => GBifunctor (K1 i (f c c)) (K1 i (f d d)) a b c d where
  gbimap' _ g (K1 b) = K1 (bimap g g b)


instance {-# INCOHERENT #-} (Bifoldable f , Semigroup m , Monoid m)
  => GBifoldable (K1 i (f a b)) a b m where
  gbifoldMap' f g (K1 a) = bifoldMap f g a

instance {-# INCOHERENT #-} (Foldable f , Semigroup m , Monoid m)
  => GBifoldable (K1 i (f b)) a b m where
  gbifoldMap' _ g (K1 a) = foldMap g a 

instance {-# INCOHERENT #-} (Foldable f , Semigroup m , Monoid m)
  => GBifoldable (K1 i (f a)) a b m where
  gbifoldMap' f _ (K1 a) = foldMap f a

instance {-# INCOHERENT #-} (Bifoldable f , Semigroup m , Monoid m)
  => GBifoldable (K1 i (f a a)) a b m where
  gbifoldMap' f _ (K1 a) = bifoldMap f f a

instance {-# INCOHERENT #-} (Bifoldable f , Semigroup m , Monoid m)
  => GBifoldable (K1 i (f b b)) a b m where
  gbifoldMap' _ g (K1 b) = bifoldMap g g b


instance {-# INCOHERENT #-} Bitraversable f
  => GBitraversable (K1 i (f a c)) (K1 i (f b d)) a b c d where
  gbitraverse' f g (K1 a) = K1 <$> bitraverse f g a

instance {-# INCOHERENT #-} Traversable f
  => GBitraversable (K1 i (f c)) (K1 i (f d)) a b c d where
  gbitraverse' _ g (K1 a) = K1 <$> traverse g a

instance {-# INCOHERENT #-} Traversable f
  => GBitraversable (K1 i (f a)) (K1 i (f b)) a b c d where
  gbitraverse' f _ (K1 a) = K1 <$> traverse f a

instance {-# INCOHERENT #-} Bitraversable f
  => GBitraversable (K1 i (f a a)) (K1 i (f b b)) a b c d where
  gbitraverse' f _ (K1 a) = K1 <$> bitraverse f f a

instance {-# INCOHERENT #-} Bitraversable f
  => GBitraversable (K1 i (f c c)) (K1 i (f d d)) a b c d where
  gbitraverse' _ g (K1 b) = K1 <$> bitraverse g g b


gbimap :: ( Generic (p a c)
          , Generic (p b d)
          , GBifunctor (Rep (p a c)) (Rep (p b d)) a b c d
          ) => (a -> b) -> (c -> d) -> p a c -> p b d
gbimap f g = to . gbimap' f g . from

gbifoldMap :: ( Generic (p a b)
              , GBifoldable (Rep (p a b)) a b m
              ) => (a -> m) -> (b -> m) -> p a b -> m
gbifoldMap f g = gbifoldMap' f g . from

gbitraverse :: ( Generic (p a c)
               , Generic (p b d)
               , GBitraversable (Rep (p a c)) (Rep (p b d)) a b c d
               , Applicative f
               ) => (a -> f b) -> (c -> f d) -> p a c -> f (p b d)
gbitraverse f g = fmap to . gbitraverse' f g . from

