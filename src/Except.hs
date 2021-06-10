{-# LANGUAGE DeriveFunctor #-}
module Except where


import Free

newtype Except_ e except = Throw e deriving(Functor)

type ExceptT e = FreeT (Except_ e)
type Except e a = Free (Except_ e) a

throw :: e -> ExceptT e m a
throw = liftFree . Throw

catch :: Functor m => (e -> ExceptT e m a) -> ExceptT e m a -> ExceptT e m a
catch handler = higher $ \case
    Throw e -> handler e
