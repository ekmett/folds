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
