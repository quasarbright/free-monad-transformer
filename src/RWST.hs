{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RWST where

import Free ( liftFree, FreeT(..), higher, Free )
import Control.Monad.Reader.Class ( MonadReader(local, ask) )
import Control.Monad.Writer.Class
    ( MonadWriter(pass, listen, tell) )
import Control.Arrow (second)
import Control.Monad.State.Class ( MonadState(get, put) )

data RWS_ r w s rws
    = Ask (r -> rws)
    | Tell w rws
    | Get (s -> rws)
    | Put s rws
    deriving(Functor)

type RWST r w s = FreeT (RWS_ r w s)
type RWS r w s a = Free (RWS_ r w s) a

instance Functor m => MonadReader r (RWST r w s m) where
    ask = liftFree (Ask id)
    local f = let go = local f in higher $ \case
        Ask fr -> Free (Ask (fr . f))
        Tell w ff -> Free (Tell w (go ff))
        Get fs -> Free $ Get $ \s -> go (fs s)
        Put s ff -> Free $ Put s (go ff)
-- very similar to pipes. interesting

instance (Functor m, Monoid w) => MonadWriter w (RWST r w s m) where
    tell w = liftFree (Tell w ())
    listen = \case
        Pure a -> Pure (a, mempty)
        M m -> M (listen <$> m)
        Free (Tell w f) -> second (w <>) <$> listen f
        Free (Ask fr) -> Free $ Ask $ \r -> listen (fr r)
        Free (Get fs) -> Free $ Get $ \s -> listen (fs s)
        Free (Put s f) -> Free $ Put s (listen f)
    pass = \case
        Pure (a, f) -> do
            tell (f mempty)
            pure a
        M m -> M (pass <$> m)
        Free (Tell w ff) -> do
            ((a, fw), w') <- listen ff
            tell (fw (w <> w'))
            pure a
        Free (Ask fr) -> Free $ Ask $ \r -> pass (fr r)
        Free (Get fr) -> Free $ Get $ \r -> pass (fr r)
        Free (Put s f) -> Free $ Put s (pass f)

instance (Functor m) => MonadState s (RWST r w s m) where
    get = liftFree (Get id)
    put s = liftFree (Put s ())


