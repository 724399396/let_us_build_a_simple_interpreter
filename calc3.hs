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
  empty = Parser $ \s -> Nothing
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

int :: Parser Int
int = read <$> (many $ satisfy isDigit)

op :: Parser Op
op = (char '+' >> return Add)
     <|> (char '-' >> return Sub)
     <|> (char '*' >> return Mul)
     <|> (char '/' >> return Div)

data Op = Add | Sub | Mul | Div deriving (Show, Eq)
data Expr = Expr Op Expr Expr | Val Int deriving Show

parse :: Parser Expr
parse = do
  whitespace
  v <- int
  (do whitespace
      o <- op
      l <- parse
      return $ Expr o (Val v) l) <|> (return $ Val v)

eval :: Expr -> Int
eval (Val x) = x
eval (Expr o l r) | o == Add = (eval l) + (eval r)
                  | o == Sub = (eval l) - (eval r)
                  | o == Mul = (eval l) * (eval r)
                  | o == Div = (eval l) `div` (eval r)
                  
expr :: String -> String
expr ip = case runParser parse ip of
            Just (x, _) -> show $ eval x
            Nothing -> "Parser error"

main :: IO String
main = forever $
  do
    putStr "calc>"
    l <- getLine
    putStrLn $ expr l
