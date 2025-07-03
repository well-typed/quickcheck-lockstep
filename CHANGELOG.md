# Revision history for quickcheck-lockstep

## 0.8.0 -- 2025-07-03

* BREAKING: Support `quickcheck-dynamic-4`. Going from version 3 to 4, some
  breaking changes were introduced in `quickcheck-dynamic`, which cascades into
  changes for `quickcheck-lockstep`. The changes to the latter are listed below
  together with hints as to how to migrate from `quickcheck-lockstep-0.7` to
  `quickcheck-lockstep-0.8`. For migrating from `quickcheck-dynamic-3` to
  `quickcheck-dynamic-4`, see [the `quickcheck-dynamic-4`
  changelog](https://hackage.haskell.org/package/quickcheck-dynamic-4.0.0/changelog).

  - The `Realized` type family is removed.

    + Types of the form `Realized m a` are replaced by `a` everywhere.

      Users should make this change in their own code as well.

    + Types of the form `LookUp m` are replaced by `LookUp` everywhere.

      Users should make this change in their own code as well.

    + The `RealLookUp` type synonym no longer has an `m` type argument, and the
      `Proxy m` type is removed from the right-hand side of the definition.
      Types of the form `RealLookup m op` are replaced by `RealLookup op`
      everywhere.

      Users should make this change in their own code as well. (Any values of)
      the type `Proxy m` that were previously passed to functions of the
      `RealLookup` type can be removed.

    + `WrapRealized m` is replaced by `Identity`. The `WrapRealized m` type and
      its helper functions (`intOpRealizedId`, `intOpTransformer`) are removed
      completely. A new helper function called `intOpIdentity` is added. For the
      `Op` from the `SumProd` module, the instances `InterpretOp Op
      (WrapRealized IO)`, `InterpretOp Op (WrapRealized (StateT s m))`, and
      `InterpretOp Op (WrapRealized (ReaderT r m))` are removed and replaced by
      an `InterpretOp Op Identity` instance.

      Instead of providing an `InterpretOp op (WrapRealized m)` instance, users
      should now provide an `InterpretOp op Identity` instance if `op` is a
      custom type that is not included in `quickcheck-lockstep` (like
      `SumProd.Op`, which comes an instance already defined). Use
      `intOpIdentity` to help define this instance for custom `op` types.

  - The `Error` type family is moved from `StateModel` to `RunModel`, and now
    requires an additional `m` type paramer. For `quickcheck-lockstep`, types of
    the form `Error st` are replaced by `Error st m` everywhere.

    The user should make this change in their own code as well.

* PATCH: enable a bunch of GHC warnings
* PATCH: use GHC2021, and if not available, enable all GHC2021 language
  extensions explicitly.

## 0.7.0 -- 2025-05-09

* BREAKING: Rename `lookupGVar` to `realLookupVar`, and add a `RealLookup`
  convenience alias that is used in the type of `RealLookupVar`. The type of
  `realLookupVar` is slightly different than the type of `lookupGVar`, but only
  in the positions of universal quantification over types.
* NON-BREAKING: improved error messages for `realLookupVar` and `lookupVar`,
  which might throw an error when variables are ill-defined or unevaluable.
* PATCH: some documentation is moved from convenience aliases like
  `ModelFindVariables` to the functions have these aliases in their type, like
  `findVars`.
* BREAKING: Enable verbose counterexamples by default in the 'postcondition'
  function using 'postconditionWith'.
* NON-BREAKING: Add a new 'postconditionWith' function that can be configured to
  produce more verbose counterexamples. With verbosity disabled, all states of
  the model are printed in a counterexample. If verbosity is enabled, the
  counterexample will also include all responses from the real system and the
  model.
* NON-BREAKING: Add a new `shrinkVar` function and `ModelShrinkVar` type alias.
  Use `shrinkVar` to shrink variables to earlier variables of the same type.
* PATCH: allow building with `ghc-9.12`
* PATCH: support `containers-0.8`

## 0.6.0 -- 2024-12-03

* BREAKING: Generalise `ModelFindVariables` and `ModelLookup` to
  `ModelVarContext`. Occurrences of `ModelFindVariables` and `ModelLookup` in
  the `InLockstep` class are replaced by the newly exposed `ModelVarContext`. A
  `ModelFindVariables` can be recovered from a `ModelVarContext` using the new
  `findVars` functions. A `ModelLookup` can be recovered from a
  `ModelVarContext` using the new `lookupVars` function. Since these functions
  can be recovered from `ModelVarContext`, existing tests are guaranteed to be
  adaptable to the new `InLockstep` API. This breaking changes means that, for
  example ...
  ```haskell
  arbitraryWithVars lookupVars = ...
    (lookupVars ...)
  ```
  ... should be changed to ...
  ```haskell
  arbitraryWithVars vctx = ...
    (lookupVars vctx ...)
  ```

## 0.5.1 -- 2024-08-27

* PATCH: allow building with `ghc-9.10`
* PATCH: bump dependency versions for `containers` and `QuickCheck`

## 0.5.0 -- 2024-03-25

* BREAKING: Update `quickcheck-dynamic` dependency to `>=3.4.1`. The main change
  is that `quickcheck-dynamic`'s `StateModel` class now has an associated type
  `Error`, the use of which is optional. However, as a result, some functions in
  `quickcheck-lockstep` change type signatures: the default `monitoring`
  function, `runActions`, and `runActionsBracket`.

## 0.4.1 -- 2024-03-20

* PATCH: fix compilation failures when using `mtl ^>=2.3`

## 0.4.0 -- 2024-02-17

* BREAKING: Counter-examples now produce valid code. To facilitate this,
  `HasVariables` and `Show` instances have changed, and a new `unsafeMkGVar`
  smart constructor is exposed.
* Add compatibility with ghc-9.8

## 0.3.0 -- 2023-10-17

* BREAKING: Update `quickcheck-dynamic` dependency to `>=3.3`.
* Add compatibility with ghc-9.6 (Javier Sagredo)

## 0.2.1 -- 2022-12-06

* Expose necessary definitions for custom `Operation` instances (Joris Dral)

## 0.2.0 -- 2022-11-11

* Relax bounds on `base` (support up to ghc 9.4)
* Show real/mock response in addition to observable response
  (see `showRealResponse`)
* Add `labelActions` function
* Generalise `runActionsBracket` (Joris Dral)
* Expose getter for the model in `Lockstep` (Joris Dral)

## 0.1.0 -- 2022-10-11

* First release
