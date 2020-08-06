# Generic data types in Haskell [![Hackage](https://img.shields.io/hackage/v/generic-data.svg)](https://hackage.haskell.org/package/generic-data) [![GitHub CI](https://github.com/Lysxia/generic-data/workflows/CI/badge.svg)](https://github.com/Lysxia/generic-data/actions)

Utilities for `GHC.Generics`.

## Generic deriving for standard classes

### Example: generically deriving Semigroup instances for products

Semi-automatic method using `gmappend`

```haskell
data Foo a = Bar [a] [a] deriving Generic

instance Semigroup (Foo a) where
  (<>) = gmappend
```

This library also synergizes with the `DerivingVia` extension
(introduced in GHC 8.6), thanks to the `Generically` newtype.

```haskell
data Foo a = Bar [a] [a]
  deriving Generic
  deriving Semigroup via (Generically (Foo a))
```

These examples can be found in `test/example.hs`.

---

Note for completeness, the first example uses the following extensions and
imports:

```haskell
{-# LANGUAGE DeriveGeneric #-}

-- base
import Data.Semigroup (Semigroup(..))
import GHC.Generics

-- generic-data
import Generic.Data (gmappend)
import Generic.Data.Orphans ()
```

The second example makes these additions on top:

```haskell
{-# LANGUAGE
    DerivingStrategies,
    DerivingVia #-}  -- since GHC 8.6.1

-- In addition to the previous imports
import Generic.Data (Generically(..))
```

### Supported classes

Supported classes that GHC currently can't derive: `Semigroup`, `Monoid`,
`Applicative`, `Alternative`, `Eq1`, `Ord1`, `Show1`.

Other classes from base are also supported, even though GHC can already derive
them:

- `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read` (derivable by the standard);
- `Functor`, `Foldable`, `Traversable` (derivable via extensions,
  `DeriveFunctor`, etc.).

To derive type classes outside of the standard library, it might be worth
taking a look at [one-liner](https://hackage.haskell.org/package/one-liner).

## Type metadata

Extract type names, constructor names, number and arities of constructors, etc..

## Type surgery

generic-data offers simple operations (microsurgeries) on generic
representations.

More surgeries can be found in
[generic-data-surgery](https://hackage.haskell.org/package/generic-data-surgery),
and suprisingly, in
[generic-lens](https://hackage.haskell.org/package/generic-lens) and
[one-liner](https://hackage.haskell.org/package/one-liner).

For more details, see also:

- the module `Generic.Data.Microsurgery`;

- the files `test/lens-surgery.hs` and `one-liner-surgery.hs`.

### Surgery example

Derive an instance of `Show` generically for a record type,
but as if it were not a record.

```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generic (Generic)
import Generic.Data (gshowsPrec)
import Generic.Data.Microsurgery (toData, derecordify)

-- An example record type
newtype T = T { unT :: Int } deriving Generic

-- Naively deriving Show would result in this being shown:
--
-- show (T 3) = "T {unT = 3}"
--
-- But instead, with a simple surgery, unrecordify, we can forget T was
-- declared as a record:
--
-- show (T 3) = "T 3"

instance Show T where
  showsPrec n = gshowsPrec n . derecordify . toData

-- This example can be found in test/microsurgery.hs
```

Alternatively, using `DerivingVia`:

```haskell
{-# LANGUAGE DeriveGeneric, DerivingVia #-}
import GHC.Generic (Generic)

-- Constructors must be visible to use DerivingVia
import Generic.Data.Microsurgery (Surgery, Surgery'(..), Generically(..), Derecordify)

data V = V { v1 :: Int, v2 :: Int }
  deriving Generic
  deriving Show via (Surgery Derecordify V)

-- show (V {v1 = 3, v2 = 4}) = "V 3 4"
```

---

## Related links

generic-data aims to subsume generic deriving features of the following
packages:

- [semigroups](https://hackage.haskell.org/package/semigroups): generic
  `Semigroup`, `Monoid`, but with a heavier dependency footprint.
- [transformers-compat](https://hackage.haskell.org/package/transformers-compat):
  generic `Eq1`, `Ord1`, `Show1`.
- [generic-deriving](https://hackage.haskell.org/package/generic-deriving):
  doesn't derive the classes in base (defines clones of these classes as a toy
  example); has Template Haskell code to derive `Generic` (not in generic-data).

Other relevant links.

- [deriving-compat](https://hackage.haskell.org/package/deriving-compat):
  deriving with Template Haskell.
- [one-liner](https://hackage.haskell.org/package/one-liner): another approach
  to using `GHC.Generics` to derive instances of many type classes, including
  but not restricted to the above classes (this is done in
  [one-liner-instances](https://hackage.haskell.org/package/one-liner-instances)).
- [singletons](https://hackage.haskell.org/package/singletons),
  [first-class-families](https://hackage.haskell.org/package/first-class-families)
  (second one written by me)
  libraries for dependently-typed programming in Haskell.

---

## Internal module policy

Modules under `Generic.Data.Internal` are not subject to any versioning policy.
Breaking changes may apply to them at any time.

If something in those modules seems useful, please report it or create a pull
request to export it from an external module.

---

All contributions are welcome. Open an issue or a pull request on Github!
