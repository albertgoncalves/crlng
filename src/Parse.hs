module Parse where

import Ast (Expr (..), Func (..), Scope (..), Stmt (..))
import Control.Monad (void)
import Data.Char (isDigit, isLower, isSpace)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    get,
    look,
    munch,
    munch1,
    pfail,
    readP_to_S,
    satisfy,
    string,
    (<++),
  )

many :: ReadP a -> ReadP [a]
many p = many1 p <++ return []

many1 :: ReadP a -> ReadP [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x : xs

choice :: [ReadP a] -> ReadP a
choice [] = pfail
choice [p] = p
choice (p : ps) = p <++ choice ps

dropThru :: String -> ReadP ()
dropThru [] = return ()
dropThru s@(c : cs) = do
  match <- (c ==) <$> get
  if match
    then void (string cs) <++ dropThru s
    else dropThru s

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
      choice
        [ '\n' <$ string "\\n",
          '\\' <$ string "\\\\",
          '"' <$ string "\\\"",
          satisfy (`notElem` "\"\\")
        ]
  _ <- char '"'
  return s

ident :: ReadP String
ident = token $ do
  c <- satisfy isLower
  cs <- munch (\x -> isLower x || isDigit x || x == '_')
  return $ c : cs

operator :: ReadP String
operator = token $ (: []) <$> choice (map char "=+-*/%")

unpackLabel :: String -> String
unpackLabel "channel" = "channel_new"
unpackLabel "ready" = "channel_ready"
unpackLabel label = label

exprCall :: ReadP Expr
exprCall = do
  _ <- token $ char '('
  label <- choice [operator, ident]
  args <- many expr
  _ <- token $ char ')'
  return $ ExprCall False (unpackLabel label) args

exprIfElse :: ReadP Expr
exprIfElse = do
  _ <- token $ string "if"
  cond <- expr
  scopeTrue <- scope
  _ <- token $ string "else"
  scopeFalse <- scope <++ (Scope [] <$> exprIfElse)
  return $ ExprIfElse cond scopeTrue scopeFalse

expr :: ReadP Expr
expr =
  choice
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
stmt = choice [stmtLet, StmtEffect <$> expr]

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
  return $
    Func (if label == "main" then "main_thread" else label) args $
      Scope body $ tailCall returnExpr

parse :: String -> [Func]
parse = fst . head . readP_to_S (many1 func <* space <* eof)
