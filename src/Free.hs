{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Free where

import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

data FreeT f m a
    = Pure a
    | M (m (FreeT f m a))
    | Free (f (FreeT f m a))

type Free f a = FreeT f Identity a

-- | Helper to interpret the free monad. The reducer is the "interpreter" of f. Handles pure and lift cases for you
foldFreeT :: (Monad m, Functor f) => (f (m a) -> m a) -> FreeT f m a -> m a
foldFreeT f = let go = foldFreeT f in \case
    Pure a -> pure a
    M m -> go =<< m
    Free ff -> f (go <$> ff)

-- | Like foldFreeT, but allows access to original computation (paramorphism)
paraFreeT :: (Monad m, Functor f) => (f (m a, FreeT f m a) -> m a) -> FreeT f m a -> m a
paraFreeT f = let go = paraFreeT f in \case
    Pure a -> pure a
    M m -> go =<< m
    Free ff -> f (help <$> ff) where help fa = (go fa, fa) -- reduce and retain original

liftFreeM :: (Functor f, Monad m) => f (m a) -> FreeT f m a
liftFreeM f = Free (fmap lift f)

liftFree :: Functor f => f a -> FreeT f m a
liftFree fa = Free (fmap Pure fa)

foldFree :: Functor f => (f a -> a) -> Free f a -> a
foldFree f ff = runIdentity $ foldFreeT f' ff
    where
        f' fa = Identity (f $ runIdentity <$> fa)

paraFree :: Functor f => (f (a, Free f a) -> a) -> Free f a -> a
paraFree f = let go = paraFree f in \case
    Pure a -> a
    M m -> runIdentity (go <$> m)
    Free ff -> f (help <$> ff) where help fa = (go fa, fa)

-- | Tool for creating higher order effects (like reader's local and except's catch)
higher :: Functor m => (f (FreeT f m a) -> FreeT f m a) -> FreeT f m a -> FreeT f m a
higher f = \case
    Pure a -> Pure a
    M m -> M (higher f <$> m)
    Free ff -> f ff

instance (Functor f, Functor m) => Functor (FreeT f m) where
    fmap f = \case
        Pure a -> Pure (f a)
        M m -> M (fmap (fmap f) m)
        Free ff -> Free (fmap (fmap f) ff)

instance (Functor f, Functor m) => Applicative (FreeT f m) where
    pure = Pure
    Pure f <*> m = f <$> m
    M m <*> f = M (fmap (<*> f) m)
    Free ff <*> f = Free (fmap (<*> f) ff)

instance (Functor f, Functor m) => Monad (FreeT f m) where
    return = pure
    Pure a >>= k = k a
    M m >>= k = M (fmap (>>= k) m)
    Free f >>= k = Free (fmap (>>= k) f)

instance (Functor f) => MonadTrans (FreeT f) where
    lift m = M (pure <$> m)
