{-# LANGUAGE DeriveFunctor #-}

module SPI where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Maybe

newtype Identity a = Identity { runIdentity :: a } deriving Functor

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Monad Identity where
  return = Identity
  ia >>= f = f $ runIdentity ia

data StateT m s a = StateT { runStateT :: s -> m (a, s) } deriving Functor

instance (Monad m) => Applicative (StateT m s) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (StateT m s) where
  return a = StateT $ \s -> return (a, s)
  sma >>= f = StateT $ \s -> do (a, s') <- runStateT sma s
                                runStateT (f a) s'

put :: (Monad m) => s -> StateT m s ()
put s = StateT $ \_ -> return ((), s)

get :: (Monad m) => StateT m s s
get = StateT $ \s -> return (s,s)

type State = StateT Identity

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

type Parser = ParserT Identity

type Identifier = String
data Op = Plus | Minus | Mul | Div deriving (Show, Eq)
data Unary = Pos | Neg deriving (Show, Eq)
data Expr = BinOp Expr Op Expr |
            UnaryOp Unary Expr |
            ExprVal Int |
            ExprVar Var
            deriving Show

type SymbolTable = [(Identifier, Int)]
data Program = Compound [Assign] deriving Show
data Assign = Assign Var Expr deriving Show
data Var = Var Identifier deriving Show

initSymbolTable = []

satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = ParserT $ \s ->
  return $ case s of
             x:xs -> if (cond x) then (Just x,xs) else (Nothing, s)
             _    -> (Nothing, s)

char :: Char -> Parser Char
char c = satisfy (c==)

ignoreCaseChar :: Char -> Parser Char
ignoreCaseChar c = satisfy (on (==) toLower c)

string :: String -> Parser String
string str = traverse char str

ignoreCaseString :: String -> Parser String
ignoreCaseString str = traverse ignoreCaseChar str

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
  whitespace
  return r

begin :: Parser ()
begin = token $ ignoreCaseString "begin" >> return ()

end :: Parser ()
end = token $ ignoreCaseString "end" >> return ()

semi :: Parser ()
semi = token $ char ';' >> return ()

assign :: Parser ()
assign = token $ string ":=" >> return ()

compoundStatement :: Parser Program
compoundStatement = do
  begin
  nodes <- statementList
  end
  return nodes

statementList :: Parser Program
statementList =
  (do Compound n <- statement
      semi
      Compound left <- statementList
      return $ Compound (n++left)) <|>
  statement

statement :: Parser Program
statement = compoundStatement
  <|> assignmentStatement
  <|> emptyStatement

emptyStatement :: Parser Program
emptyStatement = return $ Compound []

assignmentStatement :: Parser Program
assignmentStatement = do
  v <- variable
  assign
  e <- expr
  return $ Compound [Assign v e]

variable :: Parser Var
variable = do
  f <- token $ satisfy (\c -> isLetter c || c == '_')
  left <- many $ token $ satisfy isLetter
  return $ Var (f:left)

factor :: Parser Expr
factor =
  (do _ <- token $ char '('
      v <- expr
      _ <- token $ char ')'
      return v)
  <|> (token $ char '+' >> (UnaryOp Pos) <$> expr)
  <|> (token $ char '-' >> (UnaryOp Neg) <$> expr)
  <|> (ExprVar <$> variable)
  <|> (ExprVal . read) <$> (token $ many $ satisfy isDigit)

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

parse :: String -> Program
parse ip = let y = runIdentity $ runParserT program ip
           in
              case y of
                (Just x, "")    -> x
                (Just _, left)  -> error $ "not consume " ++ left
                (Nothing, left) -> error $ "error when parse: " ++ left

interpret :: Program -> State SymbolTable ()
interpret (Compound []) = return ()
interpret (Compound ((Assign (Var x) v):xs)) = do
  now <- get
  put ((x, (interpretExpr v)) : now)
  interpret (Compound xs)

interpretExpr :: Expr -> Int
interpretExpr (ExprVal x) = x
interpretExpr (BinOp l op r) = case op of
  Plus  -> interpretExpr l + interpretExpr r
  Minus -> interpretExpr l - interpretExpr r
  Mul   -> interpretExpr l * interpretExpr r
  Div   -> interpretExpr l `div` interpretExpr r
interpretExpr (UnaryOp op e) = case op of
  Pos -> interpretExpr e
  Neg -> negate $ interpretExpr e
