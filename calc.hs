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

token :: Parser a -> Parser a
token pa = whitespace >> pa

factor :: Parser Int
factor = (do token $ char '('
             v <- expr
             token $ char ')'
             return v)
  <|> read <$> (token $ many $ satisfy isDigit)

expr :: Parser Int
expr = do
  l <- term
  opt <- many (do o <- token (char '+' <|>  char '-')
                  r <- term
                  case o of
                    '+' -> return (+ r)
                    '-' -> return (\x -> x - r))
  return $ foldl (flip ($)) l opt

term :: Parser Int
term = do
  l <- factor
  opt <- many (do o <- token (char '*' <|> char '/')
                  r <- factor
                  case o of
                    '*' -> return (* r)
                    '/' -> return (`div` r))
  return $ foldl (flip ($)) l opt

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
