{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Char
import Data.Function
import Data.Maybe

newtype Identity a = Identity { runIdentity :: a } deriving Functor

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Monad Identity where
  return = Identity
  ia >>= f = f $ runIdentity ia

data ParserT m a = ParserT {runParserT :: String -> m (Maybe a,String)}
  deriving Functor

type Parser = ParserT Identity

instance (Monad m) => Applicative (ParserT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (ParserT m) where
  return a = ParserT $ \s -> return $ (Just a,s)
  mpa >>= f = ParserT $ \s -> do (pa, s') <- runParserT mpa s
                                 case pa of
                                   Just a -> runParserT (f a) s'
                                   Nothing -> return (Nothing, s')

instance (Monad m) => Alternative (ParserT m) where
  empty = ParserT $ \s -> return (Nothing,s)
  pa <|> pb = ParserT $ \s -> do w@(x,_) <- runParserT pa s
                                 case x of
                                   Nothing -> runParserT pb s
                                   x -> return w

type Identifier = String
data Op = Plus | Minus | Mul | Div deriving (Show, Eq)
data Unary = Pos | Neg deriving (Show, Eq)
data Expr = BinOp Expr Op Expr |
            UnaryOp Unary Expr |
            ExprVal Int |
            ExprVar Var
            deriving Show

data Program = Compound [Assign] deriving Show
data Assign = Assign Var Expr deriving Show
data Var = Var Identifier deriving Show

satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = ParserT $ \s ->
  return $ case s of
             x:xs -> if (cond x) then (Just x,xs) else (Nothing, s)
             _ -> (Nothing, s)

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

parse :: String -> Identity Program
parse ip = do y <- runParserT program ip
              case y of
                (Just x, "") -> return x
                (Just _, left) -> error $ "not consume " ++ left
                (Nothing, left) -> error $ "error when parse" ++ left

interpret :: Expr -> Int
interpret (ExprVal x) = x
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
    return ()
--    putStrLn $ show $ interpret $ runIdentity $ parse l
