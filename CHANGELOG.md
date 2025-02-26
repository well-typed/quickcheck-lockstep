# Revision history for quickcheck-lockstep

## ?.?.? -- ????-??-??

* BREAKING: Enable verbose counterexamples by default in the 'postcondition'
  function using 'postconditionWith'.
* NON-BREAKING: Add a new 'postconditionWith' function that can be configured to
  produce more verbose counterexamples. With verbosity disabled, all states of
  the model are printed in a counterexample. If verbosity is enabled, the
  counterexample will also include all responses from the real system and the
  model.
* PATCH: allow building with `ghc-9.12`

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
