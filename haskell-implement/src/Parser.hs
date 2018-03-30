{-# LANGUAGE DeriveFunctor #-}

module Parser  where

import           Control.Applicative
import           Control.Monad

data ParserT m a = ParserT { runParserT :: String -> m (Maybe a, String) }
  deriving Functor

instance (Monad m) => Applicative (ParserT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (ParserT m) where
  return a = ParserT $ \s -> return $ (Just a,s)
  mpa >>= f = ParserT $ \s -> do (pa, s') <- runParserT mpa s
                                 case pa of
                                   Just a  -> runParserT (f a) s'
                                   Nothing -> return (Nothing, s')

instance (Monad m) => Alternative (ParserT m) where
  empty = ParserT $ \s -> return (Nothing,s)
  pa <|> pb = ParserT $ \s -> do w@(x,_) <- runParserT pa s
                                 case x of
                                   Nothing -> runParserT pb s
                                   x       -> return w
