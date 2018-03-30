{-# LANGUAGE DeriveFunctor #-}

module State where

import           Control.Applicative
import           Control.Monad
import           Identity

data StateT s m a = StateT { runStateT :: s -> m (a, s) } deriving Functor

instance (Monad m) => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  sma >>= f = StateT $ \s -> do (a, s') <- runStateT sma s
                                runStateT (f a) s'

put :: (Monad m) => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

get :: (Monad m) => StateT s m s
get = StateT $ \s -> return (s,s)

type State s = StateT s Identity
