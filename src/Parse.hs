module Parse where

import Ast (Expr (..), Func (..), Scope (..), Stmt (..))
import Data.Char (isDigit, isLower, isSpace)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    get,
    look,
    many,
    many1,
    munch,
    munch1,
    pfail,
    readP_to_S,
    satisfy,
    string,
    (<++),
  )

firstChoice :: [ReadP a] -> ReadP a
firstChoice [] = pfail
firstChoice [p] = p
firstChoice (p : ps) = p <++ firstChoice ps

dropThru :: String -> ReadP ()
dropThru [] = return ()
dropThru (c : cs) = do
  match <- (c ==) <$> get
  if match
    then dropThru cs
    else dropThru (c : cs)

space :: ReadP ()
space = do
  rest <- look
  case rest of
    ('/' : '*' : _) -> do
      _ <- get
      _ <- get
      dropThru "*/"
      space
    ('#' : _) -> do
      _ <- get
      _ <- munch (/= '\n') *> char '\n'
      space
    (c : _) | isSpace c -> do
      _ <- get
      space
    _ -> return ()

token :: ReadP a -> ReadP a
token = (space *>)

int :: ReadP Int
int = token (read <$> munch1 isDigit)

str :: ReadP String
str = token $ do
  _ <- char '"'
  s <-
    many $
      firstChoice
        [ '\n' <$ string "\\n",
          '\\' <$ string "\\\\",
          '"' <$ string "\\\"",
          satisfy (`notElem` "\"\\")
        ]
  _ <- char '"'
  return s

ident :: ReadP String
ident = token $ do
  c <- satisfy f
  cs <- munch (\x -> f x || isDigit x)
  return $ c : cs
  where
    f x = isLower x || x == '_'

operator :: ReadP String
operator = token $ (: []) <$> firstChoice (map char "=+-")

exprCall :: ReadP Expr
exprCall = do
  _ <- token $ char '('
  label <- firstChoice [operator, ident]
  args <- many expr
  _ <- token $ char ')'
  return $ ExprCall False label args

exprIfElse :: ReadP Expr
exprIfElse = do
  _ <- token $ string "if"
  cond <- expr
  scopeTrue <- scope
  _ <- token $ string "else"
  ExprIfElse cond scopeTrue <$> scope

expr :: ReadP Expr
expr =
  firstChoice
    [ exprIfElse,
      exprCall,
      ExprInt <$> int,
      ExprStr <$> str,
      ExprVar <$> ident
    ]

scope :: ReadP Scope
scope = do
  _ <- token $ char '{'
  stmts <- many1 stmt
  (body, returnExpr) <- case reverse stmts of
    (StmtEffect returnExpr : body) -> return (reverse body, returnExpr)
    _ -> pfail
  _ <- token $ char '}'
  return $ Scope body returnExpr

stmtLet :: ReadP Stmt
stmtLet = do
  _ <- token $ string "let"
  var <- ident
  StmtLet var <$> scope

stmt :: ReadP Stmt
stmt = firstChoice [stmtLet, StmtEffect <$> expr]

tailCall :: Expr -> Expr
tailCall (ExprCall False label args) = ExprCall True label args
tailCall
  (ExprIfElse cond (Scope bodyTrue exprTrue) (Scope bodyFalse exprFalse)) =
    ExprIfElse
      cond
      (Scope bodyTrue $ tailCall exprTrue)
      (Scope bodyFalse $ tailCall exprFalse)
tailCall (ExprCall True _ _) = undefined
tailCall e = e

func :: ReadP Func
func = do
  (label : args) <- many1 ident
  (Scope body returnExpr) <- scope
  return $ Func label args (Scope body $ tailCall returnExpr)

parse :: String -> [Func]
parse = fst . head . readP_to_S (many1 func <* space <* eof)
