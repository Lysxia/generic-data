{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}

-- | Generic implementation of Read
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Read where

import Data.Coerce
import Data.Functor.Classes (Read1(..))
import Data.Functor.Identity
import Data.Proxy
import Generic.Data.Internal.Utils (isSymDataCon, isSymVar)
import GHC.Generics hiding (prec)
import GHC.Read (expectP, list)
import GHC.Show (appPrec, appPrec1)
import Text.ParserCombinators.ReadPrec
import Text.Read (Read(..), parens)
import Text.Read.Lex (Lexeme(..))

-- | Generic 'readPrec'.
--
-- @
-- instance 'Read' MyType where
--   'readPrec' = 'greadPrec'
--   'readListPrec' = 'readListPrecDefault'
-- @
greadPrec :: (Generic a, GRead0 (Rep a)) => ReadPrec a
greadPrec = to <$> gPrecRead Proxy

-- | Generic representation of 'Read' types.
type GRead0 = GRead Proxy

-- | Generic 'liftReadPrec'.
gliftReadPrec
  :: (Generic1 f, GRead1 (Rep1 f))
  => ReadPrec a -> ReadPrec [a]
  -> ReadPrec (f a)
gliftReadPrec readPrec' readList' =
  to1 <$> gPrecRead (Identity (readPrec', readList'))

-- | Generic representation of 'Data.Functor.Classes.Read1' types.
type GRead1 = GRead Identity

class GRead p f where
  gPrecRead :: p (ReadPrec a, ReadPrec [a]) -> ReadPrec (f a)

instance (GRead p f, IsNullaryDataType f) => GRead p (M1 D d f) where
  gPrecRead p = coerceM1 (parensIfNonNullary (gPrecRead p))
    where
      x :: f a
      x = undefined

      parensIfNonNullary :: ReadPrec a -> ReadPrec a
      parensIfNonNullary = if isNullaryDataType x
                              then id
                              else parens

instance (GRead p f, GRead p g) => GRead p (f :+: g) where
  gPrecRead p = fmap L1 (gPrecRead p) +++ fmap R1 (gPrecRead p)

instance (Constructor c, GReadC p c f) => GRead p (M1 C c f) where
  gPrecRead p = gPrecReadC p (conName x) (conFixity x)
    where
      x :: M1 C c f a
      x = undefined

instance GRead p V1 where
  gPrecRead _ = pfail

class IsNullaryDataType f where
  isNullaryDataType :: f a -> Bool

instance IsNullaryDataType (f :+: g) where
  isNullaryDataType _ = False

instance IsNullaryDataType (C1 c f) where
  isNullaryDataType _ = False

instance IsNullaryDataType V1 where
  isNullaryDataType _ = True

class GReadC p c f where
  gPrecReadC :: p (ReadPrec a, ReadPrec [a]) -> String -> Fixity -> ReadPrec (M1 C c f a)

instance GReadFields p f => GReadC p ('MetaCons s y 'False) f where
  gPrecReadC :: forall a. p (ReadPrec a, ReadPrec [a]) -> String -> Fixity
             -> ReadPrec (M1 C ('MetaCons s y 'False) f a)
  gPrecReadC p name fixity
    | Infix _ fy <- fixity, Branch k1 k2 <- fields
    = coerceM1 $ prec fy $ do
        k1' <- toReadPrec k1
        if isSymDataCon name
           then expectP (Symbol name)
           else mapM_ expectP ([Punc "`"] ++ identHLexemes name ++ [Punc "`"])
        k2' <- toReadPrec k2
        pure (k1' :*: k2')
    | otherwise
    = coerceM1 $ prec appPrec $ do
        readPrefixCon name
        toReadPrec fields
    where
      fields :: ReadPrecTree (f a)
      fields = gPrecReadFields p

instance GReadNamed p f => GReadC p ('MetaCons s y 'True) f where
  gPrecReadC p name _fixity = coerceM1 $ prec appPrec1 $ do
    readPrefixCon name
    readSurround '{' fields '}'
    where
      fields = gPrecReadNamed p

class GReadFields p f where
  gPrecReadFields :: p (ReadPrec a, ReadPrec [a]) -> ReadPrecTree (f a)

instance (GReadFields p f, GReadFields p g) => GReadFields p (f :*: g) where
  gPrecReadFields p = Branch (gPrecReadFields p) (gPrecReadFields p)

instance GReadSingle p f => GReadFields p (M1 S c f) where
  gPrecReadFields p = M1Leaf (step (gPrecReadSingle p))

instance GReadFields p U1 where
  gPrecReadFields _ = U1Leaf

class GReadNamed p f where
  gPrecReadNamed :: p (ReadPrec a, ReadPrec [a]) -> ReadPrec (f a)

instance (GReadNamed p f, GReadNamed p g) => GReadNamed p (f :*: g) where
  gPrecReadNamed p = do
    l <- gPrecReadNamed p
    expectP (Punc ",")
    r <- gPrecReadNamed p
    pure (l :*: r)

instance (Selector c, GReadSingle p f) => GReadNamed p (M1 S c f) where
  gPrecReadNamed p = coerceM1 $ do
    mapM_ expectP snameLexemes
    expectP (Punc "=")
    reset (gPrecReadSingle p)
    where
      x :: M1 S c f a
      x = undefined

      sname :: String
      sname = selName x

      snameLexemes :: [Lexeme]
      snameLexemes | isSymVar sname
                   = [Punc "(", Symbol sname, Punc ")"]
                   | otherwise
                   = identHLexemes sname

instance GReadNamed p U1 where
  gPrecReadNamed _ = pure U1

class GReadSingle p f where
  gPrecReadSingle :: p (ReadPrec a, ReadPrec [a]) -> ReadPrec (f a)

instance Read a => GReadSingle p (K1 i a) where
  gPrecReadSingle _ = coerceK1 readPrec
    where
      coerceK1 :: ReadPrec a -> ReadPrec (K1 i a x)
      coerceK1 = coerce

instance Read1 f => GReadSingle Identity (Rec1 f) where
  gPrecReadSingle (Identity p) = coerceRec1 (liftReadPrecCompat p)
    where
      coerceRec1 :: ReadPrec (f a) -> ReadPrec (Rec1 f a)
      coerceRec1 = coerce

instance GReadSingle Identity Par1 where
  gPrecReadSingle (Identity (readPrec', _)) = coercePar1 readPrec'
    where
      coercePar1 :: ReadPrec p -> ReadPrec (Par1 p)
      coercePar1 = coerce

instance (Read1 f, GReadSingle p g) => GReadSingle p (f :.: g) where
  gPrecReadSingle :: forall a. p (ReadPrec a, ReadPrec [a]) -> ReadPrec ((f :.: g) a)
  gPrecReadSingle p = coerceComp1 (liftReadPrecCompat (readPrec_, readList_))
    where
      readPrec_ :: ReadPrec (g a)
      readPrec_ = gPrecReadSingle p

      readList_ :: ReadPrec [g a]
      readList_ = list readPrec_

      coerceComp1 :: ReadPrec (f (g a)) -> ReadPrec ((f :.: g) a)
      coerceComp1 = coerce

-- Helpers

coerceM1 :: ReadPrec (f p) -> ReadPrec (M1 i c f p)
coerceM1 = coerce

-- | A backwards-compatible version of 'liftReadPrec'. This is needed for
-- compatibility with @base-4.9@, where 'Read1' only offers 'liftReadsPrec',
-- not 'liftReadPrec'.
liftReadPrecCompat :: Read1 f => (ReadPrec a, ReadPrec [a]) -> ReadPrec (f a)
liftReadPrecCompat (readPrec', readList') =
#if MIN_VERSION_base(4,10,0)
    liftReadPrec readPrec' readList'
#else
    readS_to_Prec (liftReadsPrec (readPrec_to_S readPrec')
                                 (readPrec_to_S readList' 0))
#endif

data ReadPrecTree a where
  U1Leaf :: ReadPrecTree (U1 a)
  M1Leaf :: ReadPrec (f a) -> ReadPrecTree (M1 i c f a)
  Branch :: ReadPrecTree (f a) -> ReadPrecTree (g a) -> ReadPrecTree ((f :*: g) a)

toReadPrec :: ReadPrecTree a -> ReadPrec a
toReadPrec U1Leaf       = pure U1
toReadPrec (M1Leaf f)   = coerceM1 f
toReadPrec (Branch f g) = (:*:) <$> toReadPrec f <*> toReadPrec g

identHLexemes :: String -> [Lexeme]
identHLexemes s | Just (ss, '#') <- snocView s = [Ident ss, Symbol "#"]
                | otherwise                    = [Ident s]

readPrefixCon :: String -> ReadPrec ()
readPrefixCon name
  | isSymDataCon name
  = readSurround '(' (expectP (Symbol name)) ')'
  | otherwise
  = mapM_ expectP (identHLexemes name)

readSurround :: Char -> ReadPrec a -> Char -> ReadPrec a
readSurround c1 r c2 = do
  expectP (Punc [c1])
  r' <- r
  expectP (Punc [c2])
  pure r'

-- Split off the last element.
snocView :: [a] -> Maybe ([a], a)
snocView [] = Nothing
snocView xs = go [] xs
  where
    -- Invariant: second arg is non-empty
    go acc [a]    = Just (reverse acc, a)
    go acc (a:as) = go (a:acc) as
    go _   []     = error "Util: snocView"
