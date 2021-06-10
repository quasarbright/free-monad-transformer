{-# LANGUAGE DeriveFunctor #-}
module State where

import Free ( FreeT(..), liftFree )
import Data.Functor.Identity ( Identity(runIdentity) )
import Control.Monad.Trans ( MonadTrans(lift) )

-- very similar to Pipe. Interesting
-- I guess it makes sense. It's a Pipe a a m r, with a different interpretation. The difference in interpretation is that a "response" (put) in state overwrites the
-- previous "response", rather than having them both exist in sequence, waiting to be "requested"
data State_ s state
    = Get (s -> state)
    | Put s state
    deriving(Functor)

type StateT s = FreeT (State_ s)
type State s = StateT s Identity

get :: StateT s m s
get = liftFree (Get id)

gets :: (s -> a) -> StateT s m a
gets f = liftFree (Get f)

put :: s -> StateT s m ()
put s = liftFree (Put s ())

modify :: (s -> s) -> StateT s m ()
modify f = Free (Get (put . f))

stateT :: Monad m => (s -> m (s, a)) -> StateT s m a
stateT f = do
    s <- get
    (s', a) <- lift (f s)
    put s'
    pure a

-- interesting that you can't just use foldFree
runStateT :: Monad m => s -> StateT s m a -> m (s, a)
runStateT s = \case
    Pure a -> pure (s, a)
    M m -> runStateT s =<< m
    Free (Get fs) -> runStateT s (fs s)
    Free (Put s' state) -> runStateT s' state

runState :: s -> State s a -> (s, a)
runState s m = runIdentity (runStateT s m)
