{-# LANGUAGE DeriveFunctor #-}

module SPI where

import           Identity
import           MonadTrans
import           Parser
import           State
import           Writer

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Function
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Text.Printf


type Parser = ParserT Identity

type Identifier = String
type TypeDecl = String
data Op = Plus | Minus | Mul | Div | IntDiv deriving (Show, Eq)
data Unary = Pos | Neg deriving (Show, Eq)
data Expr = BinOp Expr Op Expr |
            UnaryOp Unary Expr |
            ExprInt Int |
            ExprDouble Double |
            ExprVar Var
            deriving Show

data Symbol = BuiltinTypeSymbol TypeSpec
                | IntegerSymbol Identifier Integer
                | RealSymbol Identifier Double

getBultinSymbol :: Symbol -> Maybe TypeSpec
getBultinSymbol (BuiltinTypeSymbol t) = Just t
getBultinSymbol _                     = Nothing

getValueSymbol :: Symbol -> Double
getValueSymbol (IntegerSymbol _ v) =  fromIntegral v
getValueSymbol (RealSymbol _ v) =  v
getValueSymbol (BuiltinTypeSymbol t) = error $ printf "builtin symbol %s no value" (show t)

instance Show Symbol where
  show (BuiltinTypeSymbol t) = show t
  show (IntegerSymbol i _)   = printf "<%s:Integer>" (show i)
  show (RealSymbol i _)      = printf "<%s:Real>" (show i)

class ShowValue a where
  showValue :: a -> String

instance ShowValue Symbol where
  showValue (BuiltinTypeSymbol t) = "builtin type"
  showValue (IntegerSymbol _ x)   = show x
  showValue (RealSymbol _ x)      = show x

type SymbolTable = Map.HashMap Identifier Symbol
data Program = Program Var Block deriving Show
data Block = Block Declarations CompoundStatement deriving Show
data Declarations = Declarations [VariableDeclaration] [ProcedureDeclaration] deriving Show
data VariableDeclaration = VariableDeclaration Var TypeDecl deriving Show
data ProcedureDeclaration = ProcedureDeclaration Var Block deriving Show
data CompoundStatement = CompoundStatement [Assign] deriving Show
data TypeSpec = TInteger | TReal deriving Show
data Assign = Assign Var Expr deriving Show
data Var = Var Identifier deriving Show

initSymbolTable = Map.fromList [("integer", BuiltinTypeSymbol TInteger)
                               ,("real", BuiltinTypeSymbol TReal)]

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
  vars <- join <$> (many $ (do v <- variableDeclarations
                               semi
                               return v))
  procedures <- many $ (do _ <- token $ ignoreCaseString "procedure"
                           n <- variable
                           semi
                           b <- block
                           semi
                           return $ ProcedureDeclaration n b)
  return $ Declarations vars procedures

variableDeclarations :: Parser [VariableDeclaration]
variableDeclarations = do
  f <- variable
  left <- many $ (comma >> variable)
  colon
  t <- typeSpec
  return $ map (\x -> VariableDeclaration x t) (f:left)

typeSpec :: Parser TypeDecl
typeSpec = token $ some $ satisfy isLetter

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

type Interpreter = WriterT [String] (State SymbolTable)

interpret :: Program -> Interpreter ()
interpret (Program _ (Block (Declarations vars procedures) (CompoundStatement statements))) = mapM_ interpretDec vars >> mapM_ interpretAssign statements
  where interpretAssign (Assign (Var x) v) = do
          now <- lift get
          when (not $ Map.member x now) (error $ printf "variable %s not defined before used" x)
          symbolLookup x
          value <- interpretExpr v
          lift $ put (Map.adjust (\v -> setValue v value) (map toLower x) now)

        setValue (IntegerSymbol i _) v = IntegerSymbol i (truncate v)
        setValue (RealSymbol i _) v    = RealSymbol i v

        interpretDec (VariableDeclaration (Var x) t) = do symbolDec <- symbolLookup t
                                                          let s = getBultinSymbol symbolDec
                                                          when (not $ isJust s) (error $ "no builtin type " ++ t)
                                                          symbolDefine x (case s of
                                                                             Just TInteger -> IntegerSymbol x undefined
                                                                             Just TReal -> RealSymbol x undefined)

symbolLookup :: Identifier -> Interpreter Symbol
symbolLookup x = do tell [printf "Lookup: %s" x]
                    st <- lift get
                    case Map.lookup (map toLower x) st of
                         Just x  -> return x
                         Nothing -> error $ "Symbol(identifier) not found: " ++ x

symbolDefine :: Identifier -> Symbol -> Interpreter ()
symbolDefine n s = lift get >>= \table -> do
  when (Map.member n table) $ error $ printf "Duplicate identifier %s found" n
  lift $ put $ Map.insert (map toLower n) s table
  tell [printf "Define: %s" (show s)]

interpretExpr :: Expr -> Interpreter Double
interpretExpr (ExprInt x) = return $ fromIntegral x
interpretExpr (ExprDouble x) = return x
interpretExpr (ExprVar (Var x)) = symbolLookup x >>= return . getValueSymbol
interpretExpr (BinOp l op r) = case op of
  Plus  -> liftA2 (+) (interpretExpr l) (interpretExpr r)
  Minus -> liftA2 (-) (interpretExpr l) (interpretExpr r)
  Mul   -> liftA2 (*) (interpretExpr l) (interpretExpr r)
  Div   -> liftA2 (/) (interpretExpr l) (interpretExpr r)
  IntDiv -> liftA2 (\x y -> fromIntegral $ (truncate x) `div` (truncate y)) (interpretExpr l) (interpretExpr r)
interpretExpr (UnaryOp op e) = case op of
  Pos -> interpretExpr e
  Neg -> negate <$> interpretExpr e

run :: String -> (((), [String]), SymbolTable)
run c = runIdentity $ (runStateT (runWriterT (interpret (parse c))) initSymbolTable)
