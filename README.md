# Generic data types in Haskell

Utilities for `GHC.Generics`.

## Generic deriving for standard classes

Supported classes that GHC currently can't derive: `Semigroup`, `Monoid`,
`Applicative`, `Alternative`, `Eq1`, `Ord1`, `Show1`.

Other classes from base are also supported, even though GHC can already derive
them:

- `Eq`, `Ord`, `Enum`, `Bounded`, `Show` (standard);
- `Functor`, `Foldable`, `Traversable` (via extensions, `DeriveFunctor`, etc.).

To derive type classes defined elsewhere, it might be worth taking a look at
[one-liner](https://hackage.haskell.org/package/one-liner).

## Type metadata

Extract type names, constructor names, number and arities of constructors, etc..

---

## Related links

generic-data aims to subsume the following packages, which may still be
useful for old versions of GHC and base not supported by generic-data.

- [semigroups](https://hackage.haskell.org/package/semigroups): generic
  `Semigroup`, `Monoid`.
- [transformers-compat](https://hackage.haskell.org/package/transformers-compat):
  generic `Eq1`, `Ord1`, `Show1`, `Read1`.
- [generic-deriving](https://hackage.haskell.org/package/generic-deriving):
  doesn't derive the classes in base (defines clones of these classes as a toy
  example); has Template Haskell code to derive `Generic`.

Here are other relevant links.

- [deriving-compat](https://hackage.haskell.org/package/deriving-compat):
  deriving with Template Haskell.
- [one-liner](https://hackage.haskell.org/package/one-liner): another approach
  to using `GHC.Generics` to derive instances of many type classes, including
  but not restricted to the above classes (this is done in
  [one-liner-instances](https://hackage.haskell.org/package/one-liner-instances)).
- [singletons](https://hackage.haskell.org/package/singletons):
  generic-data borrows code from this package to implement defunctionalization.

---

All contributions are welcome. Open an issue or a pull request on Github!
