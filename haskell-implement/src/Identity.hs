{-# LANGUAGE DeriveFunctor #-}

module Identity (Identity, runIdentity) where

import           Control.Applicative
import           Control.Monad

newtype Identity a = Identity { runIdentity :: a } deriving Functor

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Monad Identity where
  return = Identity
  ia >>= f = f $ runIdentity ia
