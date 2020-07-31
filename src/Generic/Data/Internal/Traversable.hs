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

{-# LANGUAGE
  DataKinds,
  DeriveFunctor,
  EmptyCase,
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  KindSignatures,
  MultiParamTypeClasses,
  RankNTypes,
  ScopedTypeVariables,
  TypeApplications,
  TypeOperators
#-}

module Generic.Data.Internal.Traversable where

import Control.Applicative (liftA2, liftA3)
import Data.Kind (Type)
import Data.Monoid
import GHC.Generics

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
  :: (Generic1 f, GTraversable Kleisli (Rep1 f), Applicative m)
  => (a -> m b) -> f a -> m (f b)
gtraverse = \f -> lowerSAp . fmap to1 . gtraverse_ (Kleisli f) . from1
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
  :: (Generic1 f, GTraversable Equal (Rep1 f), Applicative m)
  => f (m a) -> m (f a)
gsequenceA = lowerSAp . fmap to1 . gtraverse_ Refl . from1
{-# INLINE gsequenceA #-}

-- Implementation

-- * Lightweight staging with an applicative transformer
--
-- An "applicative transformer" maps applicative functors to applicative
-- functors, like a "monad transformer" maps monads to monads.
--
-- We define an applicative transformer which will reassociate and simplify
-- applicative operations at compile-time, during Core-to-Core simplification
-- passes.
--
-- There are two parts to this type:
--
-- - We first define another intermediate applicative transformer,
--   'Aps', which maintains a sequence of @f@ actions in a left-nested
--   composition using @('<*>')@.
--
-- - 'SAp' extends 'Aps' to handle the special cases of sequences of length 0
--   and 1, and also the first two elements of longer sequences, which will use
--   'liftA2' instead of @('<*>')@.
--
-- Once we have 'SAp', we can define 'GTraversable' specialized with that
-- transformer. The code is almost identical to 'Traversable' instances, except
-- for the fields, which require wrapping using 'liftSAp'.

-- | An applicative transformer which accumulates @f@-actions in
-- a left-nested composition using @('<*>')@. It's called 'Aps' because
-- there are many @ap@ (the other name for @('<*>')@).
--
-- More precisely, 'Aps' represents a sequence of @f@-actions
-- @u1 :: f x1@, ... @un :: f xn@ as "term with a hole"
-- @(_ <*> u1 <*> ... <*> un) :: f r@.
-- That hole must have type  @_ :: f (x1 -> ... -> un -> r)@;
-- we hide the variadicity using continuation-passing style ('Yoneda').
--
-- Its 'Functor' and 'Applicative' instances have no constraints.
-- 'liftAps' is the only place where an @'Applicative' f@ constraint is needed.
-- That ensures that all @f@-actions are syntactically operands to @('<*>')@.
newtype Aps f a = Aps (forall r. Yoneda f (a -> r) -> f r)

liftAps :: Applicative f => f a -> Aps f a
liftAps u = Aps (\(Yoneda t) -> t id <*> u)
{-# INLINE liftAps #-}

lowerAps :: Yoneda f (b -> c) -> Aps f b -> f c
lowerAps u (Aps v) = v u
{-# INLINE lowerAps #-}

instance Functor (Aps f) where
  fmap f (Aps u) = Aps (\t -> u (fmap (. f) t))
  {-# INLINE fmap #-}

instance Applicative (Aps f) where
  pure x = Aps (\(Yoneda t) -> t (\k -> k x))
  Aps uf <*> Aps ux = Aps (\t -> ux (Yoneda (\c -> uf (fmap (\d e -> c (d . e)) t))))
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Free functor. A delayed application of 'fmap' which can be fused with
-- an inner 'fmap' or 'liftA2'.
newtype Yoneda f a = Yoneda (forall x. (a -> x) -> f x)
  deriving Functor

-- | An applicative transformer which accumulates @f@ actions with @('<*>')@,
-- using 'Aps', but then also handles in a special way the first two elements
-- of the sequence with 'liftA2' (or fewer if the sequence is too short).
--
-- It's called 'SAp' because it's an Applicative transformer used for Staged
-- meta-programming.
data SAp f a where
  Pure :: a -> SAp f a
  FmapLift :: (x -> a) -> f x -> SAp f a
  LiftA2Aps :: (x -> y -> z -> a) -> f x -> f y -> Aps f z -> SAp f a

liftSAp :: f a -> SAp f a
liftSAp = FmapLift id
{-# INLINE liftSAp #-}

lowerSAp :: Applicative f => SAp f a -> f a
lowerSAp (Pure x) = pure x
lowerSAp (FmapLift f u) = fmap f u
lowerSAp (LiftA2Aps f u v w) =
   lowerAps (Yoneda (\k -> liftA2 (\x y -> k (f x y)) u v)) w
{-# INLINE lowerSAp #-}

instance Functor (SAp f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (FmapLift g u) = FmapLift (f . g) u
  fmap f (LiftA2Aps g u v w) = LiftA2Aps ((fmap . fmap . fmap) f g) u v w
  {-# INLINE fmap #-}

instance Applicative f => Applicative (SAp f) where
  pure = Pure
  Pure f <*> ux = fmap f ux
  uf <*> Pure x = fmap ($ x) uf
  FmapLift f ux <*> FmapLift g uy = LiftA2Aps (\x y _ -> f x (g y)) ux uy (pure ())
  FmapLift f ux <*> LiftA2Aps g u v w =
    LiftA2Aps (\x y (z, zz) -> f x (g y z zz)) ux u (liftA2 (,) (liftAps v) w)
  LiftA2Aps f u v w <*> ww =
    LiftA2Aps (\x y (z, zz) -> f x y z zz) u v (liftA2 (,) w (sap2aps ww))
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

sap2aps :: Applicative f => SAp f a -> Aps f a
sap2aps (Pure x) = pure x
sap2aps (FmapLift f u) = fmap f (liftAps u)
sap2aps (LiftA2Aps f u v w) = liftA3 f (liftAps u) (liftAps v) w
{-# INLINE sap2aps #-}

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

class GFoldable t where
  gfoldMap_ :: Monoid m => (a -> m) -> t a -> EndoM m

instance GFoldable f => GFoldable (M1 i c f) where
  gfoldMap_ f (M1 x) = gfoldMap_ f x
  {-# INLINE gfoldMap_ #-}

instance (GFoldable f, GFoldable g) => GFoldable (f :+: g) where
  gfoldMap_ f (L1 x) = gfoldMap_ f x
  gfoldMap_ f (R1 y) = gfoldMap_ f y
  {-# INLINE gfoldMap_ #-}

instance (GFoldable f, GFoldable g) => GFoldable (f :*: g) where
  gfoldMap_ f (x :*: y) = gfoldMap_ f x `mappend` gfoldMap_ f y
  {-# INLINE gfoldMap_ #-}

instance GFoldable U1 where
  gfoldMap_ _ _ = mempty
  {-# INLINE gfoldMap_ #-}

instance GFoldable V1 where
  gfoldMap_ _ v = case v of {}
  {-# INLINE gfoldMap_ #-}

instance GFoldable (K1 i a) where
  gfoldMap_ _ (K1 _) = mempty
  {-# INLINE gfoldMap_ #-}

instance GFoldable Par1 where
  gfoldMap_ f (Par1 x) = liftEndoM (f x)
  {-# INLINE gfoldMap_ #-}

instance Foldable t => GFoldable (Rec1 t) where
  gfoldMap_ f (Rec1 x) = liftEndoM (foldMap f x)
  {-# INLINE gfoldMap_ #-}

instance (Foldable t, Foldable f) => GFoldable (t :.: f) where
  gfoldMap_ f (Comp1 x) = liftEndoM (foldMap (foldMap f) x)
  {-# INLINE gfoldMap_ #-}


-- ** Traversable

data Equal (f :: Type -> Type) a b where
  Refl :: Equal f (f b) b

newtype Kleisli f a b = Kleisli (a -> f b)

class GTraversable arr t where
  gtraverse_ :: Applicative f => arr f a b -> t a -> SAp f (t b)

instance GTraversable arr f => GTraversable arr (M1 i c f) where
  gtraverse_ f (M1 x) = M1 <$> gtraverse_ f x
  {-# INLINE gtraverse_ #-}

instance (GTraversable arr f, GTraversable arr g) => GTraversable arr (f :+: g) where
  gtraverse_ f (L1 x) = L1 <$> gtraverse_ f x
  gtraverse_ f (R1 y) = R1 <$> gtraverse_ f y
  {-# INLINE gtraverse_ #-}

instance (GTraversable arr f, GTraversable arr g) => GTraversable arr (f :*: g) where
  gtraverse_ f (x :*: y) = liftA2 (:*:) (gtraverse_ f x) (gtraverse_ f y)
  {-# INLINE gtraverse_ #-}

instance GTraversable arr U1 where
  gtraverse_ _ _ = pure U1
  {-# INLINE gtraverse_ #-}

instance GTraversable arr V1 where
  gtraverse_ _ v = case v of {}
  {-# INLINE gtraverse_ #-}

instance GTraversable arr (K1 i a) where
  gtraverse_ _ (K1 x) = pure (K1 x)
  {-# INLINE gtraverse_ #-}

-- traverse

instance GTraversable Kleisli Par1 where
  gtraverse_ (Kleisli f) (Par1 x) = Par1 <$> liftSAp (f x)
  {-# INLINE gtraverse_ #-}

instance Traversable t => GTraversable Kleisli (Rec1 t) where
  gtraverse_ (Kleisli f) (Rec1 x) = Rec1 <$> liftSAp (traverse f x)
  {-# INLINE gtraverse_ #-}

-- Oh no, the encoding with @(':.:')@ is quite broken.
--
-- @t1 (... (tn (t a)) ...)@ is represented as:
-- @(t1 :.: (... :.: (tn :.: Rec1 t) ...)) a@
-- but it would be more efficient to associate to the left:
-- @(((... (Rec1 t1 :.: t2) :.: ...) :.: tn) :.: t) a
instance (Traversable t, Traversable f) => GTraversable Kleisli (t :.: f) where
  gtraverse_ (Kleisli f) (Comp1 x) = Comp1 <$> liftSAp (traverse (traverse f) x)
  {-# INLINE gtraverse_ #-}

-- sequenceA

instance GTraversable Equal Par1 where
  gtraverse_ Refl (Par1 x) = Par1 <$> liftSAp x
  {-# INLINE gtraverse_ #-}

instance Traversable t => GTraversable Equal (Rec1 t) where
  gtraverse_ Refl (Rec1 x) = Rec1 <$> liftSAp (sequenceA x)
  {-# INLINE gtraverse_ #-}

instance (Traversable t, Traversable f) => GTraversable Equal (t :.: f) where
  gtraverse_ Refl (Comp1 x) = Comp1 <$> liftSAp (traverse sequenceA x)
  {-# INLINE gtraverse_ #-}
