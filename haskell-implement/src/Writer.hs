{-# LANGUAGE DeriveFunctor #-}

module Writer where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid

data WriterT w m a = WriterT { runWriterT :: m (a, w) } deriving Functor

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
  pure = return
  (<*>) = ap

instance (Monad m, Monoid w) => Monad (WriterT w m) where
  return a = WriterT $ return (a, mempty)
  wma >>= f = WriterT $ do (a, w') <- runWriterT wma
                           (a', w'') <- runWriterT (f a)
                           return (a', w' <> w'')

tell :: (Monad m, Monoid w) => w -> WriterT w m ()
tell w = WriterT $ return ((), w)
