{-# LANGUAGE
  FlexibleInstances,
  TemplateHaskell #-}

module Inspection.Boilerplate where

import Control.Applicative (liftA2)
import Language.Haskell.TH

import Generic.Data

{-
eqEmpty, eqEmpty_, eqEmptyG :: Empty a -> Empty a -> Bool
eqEmpty_ = \_ _ -> True
eqEmpty = (==)
eqEmptyG = geq
-}

class AppendQ q where
  ($++) :: q -> DecsQ -> DecsQ

  infixr 2 $++

instance AppendQ (Q Dec) where
  ($++) = liftA2 (:)

instance AppendQ (Q [Dec]) where
  ($++) = liftA2 (++)

instance AppendQ q => AppendQ [q] where
  ps $++ qs = foldr ($++) qs ps

type Top = Name -> ExpQ -> DecsQ

mk_ :: String -> Maybe Name -> Name -> (TypeQ -> TypeQ) -> Top
mk_ bname fname_ gname ty_ tname ref =
      sigD nameR ty
  $++ sigD nameG ty
  $++ funD' nameR ref
  $++ funD' nameG (varE gname)
  $++ stock
  $++ pure []
 where
  ty = do
    let f = conT tname
    ty_ f
  nameR = mkName (bname ++ nameBase tname ++ "R")  -- Reference
  nameS = mkName (bname ++ nameBase tname ++ "S")  -- Stock
  nameG = mkName (bname ++ nameBase tname ++ "G")  -- Generic
  stock = case fname_ of
    Nothing -> pure []
    Just fname ->
          sigD nameS ty
      $++ funD' nameS (varE fname)
      $++ pure []

funD' :: Name -> ExpQ -> DecQ
funD' name body = funD name [clause [] (normalB body) []]

--

newVar :: String -> Q TypeQ
newVar x = varT <$> newName x

-- Eq and Ord

-- Sometimes there isn't an Eq constraint on the parameter.
mk_eq_ :: (TypeQ -> TypeQ) -> Top
mk_eq_ = mk_ "eq" (Just '(==)) 'geq

mk_eq :: Top
mk_eq = mk_eq_ ty where
  ty f = do
    a <- newVar "a"
    [t| Eq $a => $f $a -> $f $a -> Bool |]

mk_eq' :: Top
mk_eq' = mk_eq_ ty where
  ty f = do
    a <- newVar "a"
    [t| $f $a -> $f $a -> Bool |]

-- Sometimes there isn't an Ord constraint on the parameter.
mk_compare_ :: (TypeQ -> TypeQ) -> Top
mk_compare_ = mk_ "compare" (Just 'compare) 'gcompare

mk_compare :: Top
mk_compare = mk_compare_ ty where
  ty f = do
    a <- newVar "a"
    [t| Ord $a => $f $a -> $f $a -> Ordering |]

mk_compare' :: Top
mk_compare' = mk_compare_ ty where
  ty f = do
    a <- newVar "a"
    [t| $f $a -> $f $a -> Ordering |]

-- Functor, Foldable, Traversable

mk_fmap :: Top
mk_fmap = mk_ "fmap" (Just 'fmap) 'gfmap ty where
  ty f = do
    a <- newVar "a"
    b <- newVar "b"
    [t| ($a -> $b) -> $f $a -> $f $b |]

mk_foldMap :: Top
mk_foldMap = mk_ "foldMap" (Just 'foldMap) 'gfoldMap ty where
  ty f = do
    a <- newVar "a"
    m <- newVar "m"
    [t| Monoid $m => ($a -> $m) -> $f $a -> $m |]

mk_foldr :: Top
mk_foldr = mk_ "foldr" (Just 'foldr) 'gfoldr ty where
  ty f = do
    a <- newVar "a"
    b <- newVar "b"
    [t| ($a -> $b -> $b) -> $b -> $f $a -> $b |]

mk_traverse :: Top
mk_traverse = mk_ "traverse" (Just 'traverse) 'gtraverse ty where
  ty f = do
    a <- newVar "a"
    b <- newVar "b"
    g <- newVar "g"
    [t| Applicative $g => ($a -> $g $b) -> $f $a -> $g ($f $b) |]

mk_sequenceA :: Top
mk_sequenceA = mk_ "sequenceA" (Just 'sequenceA) 'gsequenceA ty where
  ty f = do
    a <- newVar "a"
    g <- newVar "g"
    [t| Applicative $g => $f ($g $a) -> $g ($f $a) |]

-- Applicative (no stock deriving)

mk_ap :: Top
mk_ap = mk_ "ap" Nothing 'gap ty where
  ty f = do
    a <- newVar "a"
    b <- newVar "b"
    [t| $f ($a -> $b) -> $f $a -> $f $b |]

mk_liftA2 :: Top
mk_liftA2 = mk_ "liftA2" Nothing 'gliftA2 ty where
  ty f = do
    a <- newVar "a"
    b <- newVar "b"
    c <- newVar "c"
    [t| ($a -> $b -> $c) -> $f $a -> $f $b -> $f $c |]

