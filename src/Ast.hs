module Ast where

import Text.Printf (printf)

data Expr
  = ExprInt Int
  | ExprStr String
  | ExprVar String
  | ExprCall Bool String [Expr]
  | ExprIfElse Expr Scope Scope

data Stmt
  = StmtLet String Scope
  | StmtEffect Expr

data Scope = Scope [Stmt] Expr

data Func = Func String [String] Scope

showTailCall :: Bool -> String
showTailCall True = "/* tail-call */"
showTailCall False = ""

showExpr :: Int -> Expr -> String
showExpr _ (ExprInt int) = show int
showExpr _ (ExprStr str) = show str
showExpr _ (ExprVar var) = var
showExpr _ (ExprCall tailCall label []) =
  unwords [printf "(%s)" label, showTailCall tailCall]
showExpr n (ExprCall tailCall label args) =
  unwords
    [ printf "(%s %s)" label $ unwords $ map (showExpr n) args,
      showTailCall tailCall
    ]
showExpr n (ExprIfElse cond scopeTrue scopeFalse) =
  printf
    "if %s %s else %s"
    (showExpr n cond)
    (showScope n scopeTrue)
    (showScope n scopeFalse)

indent :: Int -> String
indent n = replicate (n * 4) ' '

showScope :: Int -> Scope -> String
showScope n0 (Scope body expr) =
  printf
    "{\n%s%s}"
    ( unlines $
        map (indent n1 ++) $
          map (showStmt n1) body ++ [showExpr n1 expr]
    )
    (indent n0)
  where
    n1 = n0 + 1

showStmt :: Int -> Stmt -> String
showStmt n (StmtEffect expr) = showExpr n expr
showStmt n (StmtLet var scope) = printf "let %s %s" var $ showScope n scope

instance Show Func where
  show (Func label [] scope) = printf "%s %s\n" label $ showScope 0 scope
  show (Func label args scope) =
    printf "%s %s %s\n" label (unwords args) (showScope 0 scope)
