0.2.0.0
======

* Provided `MonadTrace` instances to the `transformers` menagerie.

* Provided the various `mtl` instance to `TraceT`.

* Based `TraceT` on `ExceptT` to shed the `either` package dependency.

* Remove some things that were of limited utility which had caused
  `profunctors, kan-extensions` dependencies to be incurred.

* Added a `Monad` superclass constraint to `MonadTrace`.


0.1.0.5
=======

*   Added `Typeable`, `Generic`, and `NFData` instances for `ErrorTrace`.

