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
