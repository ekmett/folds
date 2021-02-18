0.7.6 [2021.02.17]
------------------
* Allow building with `lens-5.*`.
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

0.7.5 [2019.09.27]
------------------
* Remove the `hlint` test suite in favor of running `hlint` directly on CI.

0.7.4
-----
* Add a library dependency on the `doctests` test suite

0.7.3
-----
* Ensure that `HLint.hs` is shipped with the library

0.7.2
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0.7.1
-----
* Support `pointed` 5
* Support `transformers` 0.5
* Support `comonad` 5
* Support GHC 8
* Cleaned up the new warnings caused by `profunctors` 5.2

0.7
-----
* Folds are closed, corepresentable profunctors. This observation supplies us with many additional useful instances.

0.6.3
-------
* `reflection` 2 support
* Compiles warning-free on GHC 7.10

0.6.2
-----
* `contravariant` 1.0 support

0.6.1
-----
* Fixed bugs in several of the `Arrow` instances.

0.6
---
* Lazier `R1`.
* `MonadZip` instances

0.5.1
-----
* Lazier `R`.

0.5.0.1
-------
* Restore compatibility with GHC < 7.8

0.5
---
* `lens` 4 compatibility

0.1
---
* Repository Initialized
