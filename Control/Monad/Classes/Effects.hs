module Control.Monad.Classes.Effects where

-- | Writer effect
data EffWriter (w :: *)

-- | Reader effect
data EffReader (e :: *)

-- | Local state change effect
data EffLocal (e :: *)

-- | State effect
data EffState (s :: *)

-- | Arbitrary monadic effect
data EffExec (w :: * -> *)

-- | Except effect
data EffExcept (e :: *)

-- | Catch effect
data EffCatch (e :: *)

-- | Map Except effect
data EffMapExcept (e :: *)
