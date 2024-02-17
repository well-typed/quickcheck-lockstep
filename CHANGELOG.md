# Revision history for quickcheck-lockstep

## next release

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
