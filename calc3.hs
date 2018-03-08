{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Char

data Parser a = Parser {runParser :: (String -> Maybe (a,String))}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a,s)
  pa >>= f = Parser $ \s -> do
    (a,s') <- (runParser pa s)
    runParser (f a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  pa <|> pb = Parser $ \s -> case runParser pa s of
                                 Nothing -> runParser pb s
                                 x -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = Parser $ \s ->
  case s of
    x:xs -> if (cond x) then Just (x,xs) else Nothing
    _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (c==)

oneOf :: String -> Parser Char
oneOf strs = asum $ map char strs

whitespace :: Parser ()
whitespace = (many $ satisfy isSpace) >> return ()

factor :: Parser Int
factor = read <$> (many $ satisfy isDigit)

expr :: Parser Int
expr = do
  whitespace
  l <- term
  (do
    whitespace
    o <- (char '+' <|>  char '-')
    whitespace
    r <- expr
    case o of
      '+' -> return $ l + r
      '-' -> return $ l - r) <|> (return l)

term :: Parser Int
term = do
  whitespace
  l <- factor
  (do whitespace
      o <- (char '*' <|> char '/')
      whitespace
      r <- term
      case o of
        '*' -> return $ l * r
        '/' -> return $ l `div` r) <|> (return l)

interpreter :: String -> String
interpreter ip = case runParser expr ip of
            Just (x, _) -> show x
            Nothing -> "Parser error"

main :: IO String
main = forever $
  do
    putStr "calc>"
    l <- getLine
    putStrLn $ interpreter l
