-- | Generic implementation of 'Foldable' and 'Traversable'.
--
-- There is already a naive implementation using the generic @'Rep'@'s
-- own instances of 'Foldable' and 'Traversable'. However, deriving then
-- generates a lot of code that may not be simplified away by GHC,
-- that results in unnecessary run-time overhead.
--
-- In contrast, this implementation guarantees that the generated code is
-- identical to stock-derived instances of 'Foldable' and 'Traversable',
-- which have the following syntactic properties:
--
-- - constructors with zero fields use 'pure' once;
-- - constructors with one field use 'fmap' once;
-- - constructors with n >= 2 fields use 'liftA2' once and @('<*>')@ n-2 times.
--
-- The heavy lifting is actually done by the ap-normalize library.

{-# LANGUAGE
  DataKinds,
  EmptyCase,
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  KindSignatures,
  MultiParamTypeClasses,
  ScopedTypeVariables,
  TypeApplications,
  TypeOperators,
  UndecidableInstances,
  UndecidableSuperClasses #-}

module Generic.Data.Internal.Traversable where

import Control.Applicative (liftA2)
import Data.Kind (Type)
import Data.Monoid
import GHC.Generics

import ApNormalize

-- * Library

-- | Generic 'foldMap'.
--
-- @
-- instance 'Foldable' MyTypeF where
--   'foldMap' = 'gfoldMap'
-- @
gfoldMap :: (Generic1 f, GFoldable (Rep1 f), Monoid m) => (a -> m) -> f a -> m
gfoldMap = \f -> lowerEndoM . gfoldMap_ f . from1
{-# INLINE gfoldMap #-}

-- | Generic 'traverse'.
--
-- @
-- instance 'Traversable' MyTypeF where
--   'traverse' = 'gtraverse'
-- @
gtraverse
  :: (Generic1 f, GTraversable (Rep1 f), Applicative m)
  => (a -> m b) -> f a -> m (f b)
gtraverse = \f -> lowerAps . fmap to1 . gtraverse_ (Kleisli f) . from1
{-# INLINE gtraverse #-}

-- | Generic 'sequenceA'.
--
-- @
-- instance 'Traversable' MyTypeF where
--   'sequenceA' = 'gsequenceA'
-- @
--
-- See also 'gtraverse'.
--
gsequenceA
  :: (Generic1 f, GTraversable (Rep1 f), Applicative m)
  => f (m a) -> m (f a)
gsequenceA = lowerAps . fmap to1 . gtraverse_ Refl . from1
{-# INLINE gsequenceA #-}

-- | Class of generic representations for which 'Foldable' can be derived.
class    GFoldable_ t => GFoldable t
instance GFoldable_ t => GFoldable t

-- | Class of generic representations for which 'Traversable' can be derived.
class    GTraversable_ t => GTraversable t
instance GTraversable_ t => GTraversable t

-- | Internal definition of 'GFoldable'.
class    (GFoldMap t, Foldable t) => GFoldable_ t
instance (GFoldMap t, Foldable t) => GFoldable_ t

-- | Internal definition of 'GTraversable'.
class    (GTraverse Kleisli t, GTraverse Equal t) => GTraversable_ t
instance (GTraverse Kleisli t, GTraverse Equal t) => GTraversable_ t

-- Implementation

-- ** Foldable

-- | Isomorphic to @Maybe m@, but we need to micromanage the
-- use of Monoid vs Semigroup to match exactly the output
-- of stock deriving, for inspection testing.
data Maybe' m = Nothing' | Just' m

type EndoM m = Endo (Maybe' m)

liftEndoM :: Monoid m => m -> EndoM m
liftEndoM x = Endo app where
  app Nothing' = Just' x
  app (Just' y) = Just' (x `mappend` y)
{-# INLINE liftEndoM #-}

lowerEndoM :: Monoid m => EndoM m -> m
lowerEndoM (Endo app) = lowerMaybe (app Nothing')
{-# INLINE lowerEndoM #-}

lowerMaybe :: Monoid m => Maybe' m -> m
lowerMaybe Nothing' = mempty
lowerMaybe (Just' x) = x
{-# INLINE lowerMaybe #-}

class GFoldMap t where
  gfoldMap_ :: Monoid m => (a -> m) -> t a -> EndoM m

instance GFoldMap f => GFoldMap (M1 i c f) where
  gfoldMap_ f (M1 x) = gfoldMap_ f x
  {-# INLINE gfoldMap_ #-}

instance (GFoldMap f, GFoldMap g) => GFoldMap (f :+: g) where
  gfoldMap_ f (L1 x) = gfoldMap_ f x
  gfoldMap_ f (R1 y) = gfoldMap_ f y
  {-# INLINE gfoldMap_ #-}

instance (GFoldMap f, GFoldMap g) => GFoldMap (f :*: g) where
  gfoldMap_ f (x :*: y) = gfoldMap_ f x `mappend` gfoldMap_ f y
  {-# INLINE gfoldMap_ #-}

instance GFoldMap U1 where
  gfoldMap_ _ _ = mempty
  {-# INLINE gfoldMap_ #-}

instance GFoldMap V1 where
  gfoldMap_ _ v = case v of {}
  {-# INLINE gfoldMap_ #-}

instance GFoldMap (K1 i a) where
  gfoldMap_ _ (K1 _) = mempty
  {-# INLINE gfoldMap_ #-}

instance GFoldMap Par1 where
  gfoldMap_ f (Par1 x) = liftEndoM (f x)
  {-# INLINE gfoldMap_ #-}

instance Foldable t => GFoldMap (Rec1 t) where
  gfoldMap_ f (Rec1 x) = liftEndoM (foldMap f x)
  {-# INLINE gfoldMap_ #-}

instance (Foldable t, Foldable f) => GFoldMap (t :.: f) where
  gfoldMap_ f (Comp1 x) = liftEndoM (foldMap (foldMap f) x)
  {-# INLINE gfoldMap_ #-}


-- ** Traversable

data Equal (f :: Type -> Type) a b where
  Refl :: Equal f (f b) b

newtype Kleisli f a b = Kleisli (a -> f b)

class GTraverse arr t where
  gtraverse_ :: Applicative f => arr f a b -> t a -> Aps f (t b)

instance GTraverse arr f => GTraverse arr (M1 i c f) where
  gtraverse_ f (M1 x) = M1 <$> gtraverse_ f x
  {-# INLINE gtraverse_ #-}

instance (GTraverse arr f, GTraverse arr g) => GTraverse arr (f :+: g) where
  gtraverse_ f (L1 x) = L1 <$> gtraverse_ f x
  gtraverse_ f (R1 y) = R1 <$> gtraverse_ f y
  {-# INLINE gtraverse_ #-}

instance (GTraverse arr f, GTraverse arr g) => GTraverse arr (f :*: g) where
  gtraverse_ f (x :*: y) = liftA2 (:*:) (gtraverse_ f x) (gtraverse_ f y)
  {-# INLINE gtraverse_ #-}

instance GTraverse arr U1 where
  gtraverse_ _ _ = pure U1
  {-# INLINE gtraverse_ #-}

instance GTraverse arr V1 where
  gtraverse_ _ v = case v of {}
  {-# INLINE gtraverse_ #-}

instance GTraverse arr (K1 i a) where
  gtraverse_ _ (K1 x) = pure (K1 x)
  {-# INLINE gtraverse_ #-}

-- traverse

instance GTraverse Kleisli Par1 where
  gtraverse_ (Kleisli f) (Par1 x) = Par1 <$> liftAps (f x)
  {-# INLINE gtraverse_ #-}

instance Traversable t => GTraverse Kleisli (Rec1 t) where
  gtraverse_ (Kleisli f) (Rec1 x) = Rec1 <$> liftAps (traverse f x)
  {-# INLINE gtraverse_ #-}

-- Oh no, the encoding with @(':.:')@ is quite broken.
--
-- @t1 (... (tn (t a)) ...)@ is represented as:
-- @(t1 :.: (... :.: (tn :.: Rec1 t) ...)) a@
-- but it would be more efficient to associate to the left:
-- @(((... (Rec1 t1 :.: t2) :.: ...) :.: tn) :.: t) a
instance (Traversable t, Traversable f) => GTraverse Kleisli (t :.: f) where
  gtraverse_ (Kleisli f) (Comp1 x) = Comp1 <$> liftAps (traverse (traverse f) x)
  {-# INLINE gtraverse_ #-}

-- sequenceA

instance GTraverse Equal Par1 where
  gtraverse_ Refl (Par1 x) = Par1 <$> liftAps x
  {-# INLINE gtraverse_ #-}

instance Traversable t => GTraverse Equal (Rec1 t) where
  gtraverse_ Refl (Rec1 x) = Rec1 <$> liftAps (sequenceA x)
  {-# INLINE gtraverse_ #-}

instance (Traversable t, Traversable f) => GTraverse Equal (t :.: f) where
  gtraverse_ Refl (Comp1 x) = Comp1 <$> liftAps (traverse sequenceA x)
  {-# INLINE gtraverse_ #-}
