{-# LANGUAGE RoleAnnotations #-}
module Control.Monad.Classes.Reader where
data EffReader (e :: *)
data EffLocal (e :: *)
type role EffReader phantom
type role EffLocal phantom
