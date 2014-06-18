module Control.Monad.Classes.Effects where

-- | Writer effect
data EffWriter (w :: *)

-- | Reader effect
data EffReader (e :: *)

-- | Local state change effect
data EffLocal (e :: *)

-- | State effect
data EffState (s :: *)
