module MonadTrans where

import State
import Writer

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance (Monoid w) => MonadTrans (WriterT w) where
  lift ma = WriterT $ do a <- ma
                         return (a, mempty)

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do a <- ma
                              return (a, s)
