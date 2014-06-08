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
