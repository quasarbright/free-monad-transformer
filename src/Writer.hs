{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Writer where

import Free ( liftFree, FreeT(..), Free )
import Control.Arrow (second)
import Data.Functor.Identity ( Identity(runIdentity) )
import Control.Monad.Trans ( MonadTrans(lift) )
import Control.Monad.Writer.Class
data Writer_ w writer = Tell w writer deriving(Functor)

type WriterT w = FreeT (Writer_ w)
type Writer w a = Free (Writer_ w) a

writerT :: (Monad m, Monoid w) => m (a, w) -> WriterT w m a
writerT m = do
    (a, w) <- lift m
    tell w
    pure a

instance (Functor m, Monoid w) => MonadWriter w (FreeT (Writer_ w) m) where
    tell w = liftFree (Tell w ())
    listen = \case
        Pure a -> Pure (a, mempty)
        M m -> M (listen <$> m)
        Free (Tell w f) -> second (w <>) <$> listen f
    pass = \case
        Pure (a, f) -> do
            tell (f mempty)
            pure a
        M m -> M (pass <$> m)
        Free (Tell w ff) -> do
            ((a, fw), w') <- listen ff
            tell (fw (w <> w'))
            pure a

runWriterT :: (Monoid w, Monad m) => WriterT w m a -> m (a, w)
runWriterT = \case
    Pure a -> pure (a, mempty)
    M m -> runWriterT =<< m
    Free (Tell w f) -> second (w <>) <$> runWriterT f

runWriter :: (Monoid w) => Writer w a -> (a, w)
runWriter = runIdentity . runWriterT
