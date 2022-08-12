# 1.0.0.0

- `Generically` and `Generically1` are in *base* 4.17 (GHC 9.4.1)!

    + *generic-data* reexports `Generically` and `Generically1` if using *base* >= 4.17.
      The following instances remain as orphans: `Eq`, `Ord`, `Read`, `Show`,
      `Enum`, `Ix`, `Bounded`, `Foldable`, `Traversable`, `Read1`, `Show1`.
    + base 4.17 includes instances for the non-stock-derivable classes:
      `Semigroup` and `Monoid` for `Generically`; `Eq1`, `Ord1`, `Functor`,
      `Applicative`, and `Alternative` for `Generically1`.
    + Note: the `Semigroup` and `Monoid` instances of *base*'s `Generically`
      are those of *generic-data*'s `GenericProduct` (which is subtly different
      from `Generically`'s previous instance in *generic-data*).
    + `Generically` and `Generically1` are no longer defined using record syntax,
      so the `unGenerically`(`1`) field accessor no longer exists.
      The field accessors for `FiniteEnumeration` and `GenericProduct` were also
      removed for uniformity.

# 0.9.2.1

- No external changes.
- Use cabal-docspec instead of doctest

# 0.9.2.0

- Add instance of `Bounded` for `FiniteEnumeration` (the same as `Generically`)

# 0.9.1.0

- Fix `conIdToString` (it was completely broken)
- Add `conIdMin` and `conIdMax` representing the leftmost and rightmost
  constructors of a data type.
- Add `NonEmptyType` and `IsEmptyType` to express the constraint that
  a generic type must or must not be empty.
- Reexport `Generic` and `Generic1` for convenience.

# 0.9.0.0

- Improved definition of `gfoldMap`, `gtraverse`, and `sequenceA`.
  The optimized Core of `Traversable` instances eliminates all `GHC.Generic` instance
  boilerplate. In many cases, it is identical to the result of GHC's `DeriveFoldable`
  and `DeriveTraversable` extensions (note: this was already not a problem for
  `gfmap`).

  It's worth noting that there are currently issues with inlining which prevent
  optimizations that *generic-data* would ideally rely on.

    + The biggest issue is that GHC will not even inline the `to` and `from`
      methods of the `Generic` instance it derives for large types (this shows
      up at around 5 constructors and 10 fields, which is indeed not really
      big). This will be fixed by a patch for GHC (WIP):
      https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2965

    + There appear to be some more inlining issues beyond that (issue #40).

# 0.8.3.0

- Add generic `Read`. Thanks to RyanGlScott.

# 0.8.2.0

- Add microsurgery `CopyRep`.
- Improve documentation of `Microsurgery` module.
- Fix a bug where `gshowsPrec` would incorrectly display prefix uses of
  symbol data constructors or record selectors (e.g., `data R = (:!:) Int Int`
  or `data S = MkS { (##) :: Int -> Int }`). Thanks to RyanGlScott.
- Fix a bug where `gshowsPrec` would incorrectly display infix uses of
  alphanumeric data constructors (e.g., ```data T = Int `MkT` Int```).
  Thanks to RyanGlScott.

# 0.8.1.0

- Add `Old` type family mapping newtypes to their underlying type.

# 0.8.0.0

- Add `GenericProduct`, for deriving `via GenericProduct B` when `B` is not the
  type `A` you want the derived instance for.
  Note this used to be `Generically`'s behavior for `Monoid` before 0.7.0.0.
- Add generic implementations for `Ix`. Thanks to Topsii.

- Add `conIdNamed`, to get a `ConId` by its type-level name
- Add instance `Show (ConId a)`
- Improve type errors for deriving `Semigroup` and `Monoid` via `Generically`.
  Thanks to yairchu.

# 0.7.0.0

- Change `Monoid` instance for `Generically`, to be compatible with users'
  non-generic instances of `Semigroup`. Thanks to yairchu.
- Add `gcoerce`, `gcoerceBinop`.

# 0.6.0.1

- Fix derivation of `Show1` for `(:.:)`

# 0.6.0.0

- Add `Surgery` newtype for DerivingVia
- `Derecordify`, `Typeage`, `RenameFields`, `RenameConstrs`, `OnFields`
  are no longer type families, but defunctionalized symbols
  to be applied using `GSurgery`.

# 0.5.0.0

- Specialize `onData` to `Data`
- Add some instances for `U1` and `V1` in `Microsurgery`
- Add `OnFields` and `DOnFields` surgeries ("higher-kindification")

# 0.4.0.0

- Created `Microsurgery` module. Initial set of surgeries:

    + `Derecordify`
    + `Typeage`
    + `RenameFields`, `RenameConstrs`
    + Some doc about using generic-lens for surgeries

# 0.3.0.0

- Add generic implementations of `enumFrom`, `enumFromThen`, `enumFromTo`,
  `enumFromThenTo`. They are actually required to be explicit for correct
  `Enum` instances. Thanks to Topsii.
- Parameterize `GEnum` by a type-level option, and add `FiniteEnum` option
  to allow `Enum` to be derived for composite types. Thanks to Topsii.

# 0.2.0.0

- Remove `Generic.Data.Types.Map`
- Add `Generic.Data.Data.Types.toData` and `fromData`
- Remove `Defun` module (subsumed by `first-class-families` package)

# 0.1.1.0

- Add `gconIndex`
- Interface for constructor tags
- Type-level `Meta` accessors
- Add basic `Newtype` functions

# 0.1.0.0

Released generic-data
