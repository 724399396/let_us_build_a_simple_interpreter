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

type Identity = String
data Op = Plus | Minus | Mul | Div deriving (Show, Eq)
data Unary = Pos | Neg deriving (Show, Eq)
data Expr = BinOp Expr Op Expr |
            UnaryOp Unary Expr |
            Val Int deriving Show

data Program = Compound [Assign]
data Assign = Assign Var Expr
data Var = Var Identity

satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = Parser $ \s ->
  case s of
    x:xs -> if (cond x) then Just (x,xs) else Nothing
    _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (c==)

string :: String -> Parser String
string str = traverse char str

oneOf :: String -> Parser Char
oneOf strs = asum $ map char strs

whitespace :: Parser ()
whitespace = (many $ satisfy isSpace) >> return ()

token :: Parser a -> Parser a
token pa = whitespace >> pa

program :: Parser Program
program = do
  r <- compoundStatement
  _ <- token $ char '.'
  return r

compoundStatement :: Parser Program
compoundStatement = do
  _ <- token $ string "begin"
  nodes <- statementList
  _ <- token $ string "end"
  return nodes

statementList :: Parser Program
statementList =
  (do Compound n <- statement
      _ <- token $ char ';'
      Compound left <- statementList
      return $ Compound (n++left)) <|>
  statement

statement :: Parser Program
statement = compoundStatement
  <|> assignmentStatement
  <|> empty

assignmentStatement :: Parser Program
assignmentStatement = do
  v <- variable
  _ <- token $ string ":="
  e <- expr
  return $ Compound [Assign v e]

variable :: Parser Var
variable = do
  f <- satisfy (\c -> isLetter c || c == '_')
  left <- many $ satisfy isLetter
  return $ Var (f:left)

factor :: Parser Expr
factor =
  (do _ <- token $ char '('
      v <- expr
      _ <- token $ char ')'
      return v)
  <|> (token $ char '+' >> (UnaryOp Pos) <$> expr)
  <|> (token $ char '-' >> (UnaryOp Neg) <$> expr)
  <|> (Val . read) <$> (token $ many $ satisfy isDigit)

expr :: Parser Expr
expr = do
  l <- term
  opt <- many (do o <- token (char '+' <|>  char '-')
                  r <- term
                  case o of
                    '+' -> return (\i -> BinOp i Plus r)
                    '-' -> return (\i -> BinOp i Minus r))
  return $ foldl (flip ($)) l opt

term :: Parser Expr
term = do
  l <- factor
  opt <- many (do o <- token (char '*' <|> char '/')
                  r <- factor
                  case o of
                    '*' -> return (\i -> BinOp i Mul r)
                    '/' -> return (\i -> BinOp i Div r))
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
interpret (UnaryOp op e) = case op of
  Pos -> interpret e
  Neg -> negate $ interpret e

main :: IO String
main = forever $
  do
    putStr "spi> "
    l <- getLine
    putStrLn $ show $ interpret $ parse l
