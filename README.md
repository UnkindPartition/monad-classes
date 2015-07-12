[![Build Status](https://travis-ci.org/feuerbach/monad-classes.svg?branch=master)](https://travis-ci.org/feuerbach/monad-classes)

See [this series of articles][1] for the detailed description and motivation.

[1]: https://ro-che.info/articles/extensible-effects

This is a more flexible version of mtl, the monad transformers library.

*   You can have many layers of e.g. state transformers in your stack, and
    you don't have to explicitly lift your `get`s and `put`s, as soon as
    different state transformers carry different types of states.

    Example:

    ``` haskell
    a :: (MonadState Bool m, MonadState Int m) => m ()
    a = do
      put False -- set the boolean state
      modify (+ (1 :: Int)) -- modify the integer state
    ```

*   mtl requires *Θ(n<sup>2</sup>)* instances (like `MonadReader e (StateT s m)`);
    monad-classes requires only *Θ(n)* of them (where *n* is the number of
    different transformer types).

    If you'd like to define your own monad-classes-style class, you have to
    write much less boilerplate code.

## The status of this repo

This repo serves two purposes:

* as a reference implementation accompanying the [series of articles on
  extensible effects][1]
* as an actual implementation we use at [Signal Vine](http://signalvine.com/)

While this is technically open source software, I am not interested in maintaining this as
an active open project. In practice this means that:

* this version of monad-classes is not on hackage
* I do not accept feature requests and pull requests. The exception is outright
  bugs; if you find any, please do report them.

There is a [hackage package][2] and a [repo][3] maintained by M Farkas-Dyck.

[2]: https://hackage.haskell.org/package/monad-classes
[3]: https://github.com/strake/monad-classes.hs
