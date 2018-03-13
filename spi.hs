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

data Op = Plus | Minus | Mul | Div deriving (Show, Eq)
data Expr = BinOp Expr Op Expr |
            Val Int deriving Show

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

factor :: Parser Expr
factor = (do token $ char '('
             v <- expr
             token $ char ')'
             return v)
  <|> (Val . read) <$> (token $ many $ satisfy isDigit)

expr :: Parser Expr
expr = do
  l <- term
  opt <- many (do o <- token (char '+' <|>  char '-')
                  r <- term
                  case o of
                    '+' -> return (\l -> BinOp l Plus r)
                    '-' -> return (\l -> BinOp l Minus r))
  return $ foldl (flip ($)) l opt

term :: Parser Expr
term = do
  l <- factor
  opt <- many (do o <- token (char '*' <|> char '/')
                  r <- factor
                  case o of
                    '*' -> return (\l -> BinOp l Mul r)
                    '/' -> return (\l -> BinOp l Div r))
  return $ foldl (flip ($)) l opt

parse :: String -> Expr
parse ip = case runParser expr ip of
            Just (x, _) -> x
            Nothing -> error "Parser error"

interpret :: Expr -> Int
interpret (Val x) = x
interpret (BinOp l op r) = case op of
  Plus -> interpret l + interpret r
  Minus -> interpret l - interpret r
  Mul -> interpret l * interpret r
  Div -> interpret l `div` interpret r

main :: IO String
main = forever $
  do
    putStr "spi> "
    l <- getLine
    putStrLn $ show $ interpret $ parse l