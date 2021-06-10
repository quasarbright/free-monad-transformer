module Reader where

import Free ( FreeT(..), foldFreeT, liftFree, liftFreeM, higher )
import Data.Functor.Identity ( Identity(runIdentity) )

newtype Reader_ r reader = Ask (r -> reader) deriving(Functor)

type ReaderT r = FreeT (Reader_ r)
type Reader r = ReaderT r Identity

ask :: ReaderT r m r
ask = asks id

asks :: (r -> a) -> ReaderT r m a
asks = liftFree . Ask

readerT :: Monad m => (r -> m a) -> ReaderT r m a
readerT f = liftFreeM (Ask f)

reader :: (r -> a) -> ReaderT r m a
reader f = liftFree (Ask f)

local :: Functor m => (r -> r) -> ReaderT r m a -> ReaderT r m a
local f = higher $ \case
    Ask fr -> Free (Ask (fr . f))

runReaderT :: Monad m => r -> FreeT (Reader_ r) m a -> m a
runReaderT r = foldFreeT $ \case
    Ask fr -> fr r

runReader :: r -> Reader r a -> a
runReader r = runIdentity . runReaderT r
