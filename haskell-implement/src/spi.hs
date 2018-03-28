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
data Op = Plus | Minus | Mul | Div | IntDiv deriving (Show, Eq)
data Unary = Pos | Neg deriving (Show, Eq)
data Expr = BinOp Expr Op Expr |
            UnaryOp Unary Expr |
            ExprInt Int |
            ExprDouble Double |
            ExprVar Var
            deriving Show

type SymbolTable = [(Identifier, Double)]
data Program = Program Var Block deriving Show
data Block = Block Declarations CompoundStatement deriving Show
data Declarations = Declarations [VariableDeclaration] deriving Show
data VariableDeclaration = VariableDeclaration Var TypeSpec deriving Show
data CompoundStatement = CompoundStatement [Assign] deriving Show
data TypeSpec = TInteger | TReal deriving Show
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

whitespace1 :: Parser ()
whitespace1 = (some $ satisfy isSpace) >> return ()

comment :: Parser ()
comment = whitespace >> char '{' >> whitespace >> (many $ satisfy (/= '}')) >> char '}' >> return ()

token :: Parser a -> Parser a
token pa = many (whitespace1 <|> comment) >> pa

begin :: Parser ()
begin = token $ ignoreCaseString "begin" >> return ()

end :: Parser ()
end = token $ ignoreCaseString "end" >> return ()

semi :: Parser ()
semi = token $ char ';' >> return ()

colon :: Parser ()
colon = token $ char ':' >> return ()

comma :: Parser ()
comma = token $ char ',' >> return ()

assign :: Parser ()
assign = token $ string ":=" >> return ()

program :: Parser Program
program = do
  _<- token $ ignoreCaseString "program"
  name <- variable
  semi
  b <- block
  _ <- token $ char '.'
  token $ return ()
  return $ Program name b

block :: Parser Block
block = do
  vars <- declarations
  statements <- compoundStatement
  return $ Block vars statements

declarations :: Parser Declarations
declarations = do
  _ <- token $ ignoreCaseString "var"
  (Declarations . join) <$> (many $ (do v <- variableDeclarations
                                        semi
                                        return v))

variableDeclarations :: Parser [VariableDeclaration]
variableDeclarations = do
  f <- variable
  left <- many $ (comma >> variable)
  colon
  t <- typeSpec
  return $ map (\x -> VariableDeclaration x t) (f:left)

typeSpec :: Parser TypeSpec
typeSpec = ((token $ ignoreCaseString "integer") >> return TInteger)
        <|> ((token $ ignoreCaseString "real") >> return TReal)

compoundStatement :: Parser CompoundStatement
compoundStatement = do
  begin
  nodes <- statementList
  end
  return nodes

statementList :: Parser CompoundStatement
statementList =
  (do CompoundStatement n <- statement
      semi
      CompoundStatement left <- statementList
      return $ CompoundStatement (n++left)) <|>
  statement

statement :: Parser CompoundStatement
statement = compoundStatement
  <|> assignmentStatement
  <|> emptyStatement

emptyStatement :: Parser CompoundStatement
emptyStatement = return $ CompoundStatement []

assignmentStatement :: Parser CompoundStatement
assignmentStatement = do
  v <- variable
  assign
  e <- expr
  return $ CompoundStatement [Assign v e]

variable :: Parser Var
variable = do
  f <- token $ satisfy (\c -> isLetter c || c == '_')
  left <- many $ satisfy (\x -> isLetter x || isDigit x)
  return $ Var (f:left)

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
  opt <- many ((do o <- token (char '*' <|> char '/')
                   r <- factor
                   case o of
                     '*' -> return (\i -> BinOp i Mul r)
                     '/' -> return (\i -> BinOp i Div r))
               <|> (do _ <- token $ ignoreCaseString "div"
                       r <- factor
                       return (\i -> BinOp i IntDiv r)))
  return $ foldl (flip ($)) l opt

factor :: Parser Expr
factor =
  (do _ <- token $ char '('
      v <- expr
      _ <- token $ char ')'
      return v)
  <|> (token $ char '+' >> (UnaryOp Pos) <$> expr)
  <|> (token $ char '-' >> (UnaryOp Neg) <$> expr)
  <|> (ExprVar <$> variable)
  <|> (ExprDouble . read) <$> (token $ (do i <- some $ satisfy isDigit
                                           _ <- char '.'
                                           f <- some $ satisfy isDigit
                                           return $ i ++ '.':f))
  <|> (ExprInt . read) <$> (token $ some $ satisfy isDigit)


parse :: String -> Program
parse ip = let y = runIdentity $ runParserT program ip
           in
              case y of
                (Just x, "")    -> x
                (Just _, left)  -> error $ "not consume " ++ left
                (Nothing, left) -> error $ "error when parse: " ++ left

interpret :: Program -> State SymbolTable ()
interpret (Program _ (Block _ (CompoundStatement statements))) = mapM_ interpretAssign statements
  where interpretAssign (Assign (Var x) v) = do
          now <- get
          value <- interpretExpr v
          put ((map toLower x, value) : now)

interpretExpr :: Expr -> State SymbolTable Double
interpretExpr (ExprInt x) = return (fromIntegral x)
interpretExpr (ExprDouble x) = return x
interpretExpr (ExprVar (Var x)) = get >>= \st -> case lookup (map toLower x) st of
                                                   Just v -> return v
                                                   Nothing -> error $ "variable not found: " ++ x
interpretExpr (BinOp l op r) = case op of
  Plus  -> liftA2 (+) (interpretExpr l) (interpretExpr r)
  Minus -> liftA2 (-) (interpretExpr l) (interpretExpr r)
  Mul   -> liftA2 (*) (interpretExpr l) (interpretExpr r)
  Div   -> liftA2 (/) (interpretExpr l) (interpretExpr r)
  IntDiv -> liftA2 (\x y -> fromIntegral $ (truncate x) `div` (truncate y)) (interpretExpr l) (interpretExpr r)
interpretExpr (UnaryOp op e) = case op of
  Pos -> interpretExpr e
  Neg -> negate <$> interpretExpr e
