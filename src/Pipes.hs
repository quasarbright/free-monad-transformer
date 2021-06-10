{-# LANGUAGE DeriveFunctor #-}
module Pipes where

import Free ( FreeT(..), foldFreeT )
import Data.Void ( absurd, Void )
import Data.Foldable (traverse_)
import Control.Monad.Trans (lift)

data Pipe_ a b pipe
    = Request (a -> pipe)
    | Respond b pipe
    deriving(Functor)

type Pipe a b = FreeT (Pipe_ a b)
type Producer a = FreeT (Pipe_ () a)
type Consumer a = FreeT (Pipe_ a Void)
type Effect = FreeT (Pipe_ () Void)

respond :: b -> Pipe a b m r -> Pipe a b m r
respond b p = Free (Respond b p)

request :: (a -> Pipe a b m r) -> Pipe a b m r
request fa = Free (Request fa)

yield :: b -> Pipe a b m ()
yield b = respond b (Pure ())

await :: Pipe a b m a
await = request Pure

yieldFrom :: (Traversable f, Applicative m) => f b -> Pipe a b m ()
yieldFrom = traverse_ yield

consume :: Monad m => (b -> m ()) -> Producer b m r -> Effect m ()
consume action = \case
    Pure _ -> Pure ()
    M m -> M (consume action <$> m)
    Free (Request f) -> consume action (f ())
    Free (Respond b p) -> lift (action b) >> consume action p

runEffect :: Monad m => Effect m r -> m r
runEffect = foldFreeT $ \case
    Request f -> f ()
    Respond never _ -> absurd never

-- request something then keep yielding it
pull :: Pipe a a m r
pull = request push

-- keep yielding a
push :: a -> Pipe a a m r
push a = respond a (request push)
