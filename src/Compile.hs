module Compile where

import Ast (Expr (..), Func (..), Scope (..), Stmt (..))
import Control.Monad.State (State, modify, state)
import Data.Char (ord)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

data Inst
  = InstPush Op
  | InstPop Op
  | InstMov Op Op
  | InstCall Op
  | InstJmp Op
  | InstJnz Op
  | InstCmp Op Op
  | InstXor Op Op
  | InstAdd Op Op
  | InstSub Op Op
  | InstLabel String
  | InstRet

data Op
  = OpImm Int
  | OpReg Reg
  | OpAddrOffset AddrOffset
  | OpLabel String
  deriving (Eq)

data Reg
  = RegRsp
  | RegRbp
  | RegRax
  | RegRcx
  | RegRdx
  | RegR8
  | RegR9
  | RegR10
  | RegR11
  | RegRdi
  | RegRsi
  deriving (Eq)

data Addr
  = AddrReg Reg
  | AddrLabel String
  deriving (Eq)

data AddrOffset = AddrOffset Addr Int
  deriving (Eq)

instance Show Inst where
  show (InstPush op) = printf "\tpush %s" $ show op
  show (InstPop op) = printf "\tpop %s" $ show op
  show (InstMov opDst opSrc) = printf "\tmov %s, %s" (show opDst) (show opSrc)
  show (InstCall op) = printf "\tcall %s" $ show op
  show (InstJmp op) = printf "\tjmp %s" $ show op
  show (InstJnz op) = printf "\tjnz %s" $ show op
  show (InstCmp opDst opSrc) = printf "\tcmp %s, %s" (show opDst) (show opSrc)
  show (InstXor opDst opSrc) = printf "\txor %s, %s" (show opDst) (show opSrc)
  show (InstAdd opDst opSrc) = printf "\tadd %s, %s" (show opDst) (show opSrc)
  show (InstSub opDst opSrc) = printf "\tsub %s, %s" (show opDst) (show opSrc)
  show (InstLabel label) = printf "%s:" label
  show InstRet = "\tret"

instance Show Op where
  show (OpImm x) = show x
  show (OpReg reg) = show reg
  show (OpAddrOffset addrOffset) = printf "qword [%s]" $ show addrOffset
  show (OpLabel label) = label

instance Show Reg where
  show RegRsp = "rsp"
  show RegRbp = "rbp"
  show RegRax = "rax"
  show RegRcx = "rcx"
  show RegRdx = "rdx"
  show RegR8 = "r8"
  show RegR9 = "r9"
  show RegR10 = "r10"
  show RegR11 = "r11"
  show RegRdi = "rdi"
  show RegRsi = "rsi"

instance Show Addr where
  show (AddrReg reg) = show reg
  show (AddrLabel label) = label

instance Show AddrOffset where
  show (AddrOffset addr 0) = show addr
  show (AddrOffset addr offset)
    | offset < 0 = printf "%s - %d" (show addr) $ abs offset
    | otherwise = printf "%s + %d" (show addr) offset

data Compiler = Compiler
  { compilerInsts :: [Inst],
    compilerStrings :: M.Map String String,
    compilerLocals :: [M.Map String Int],
    compilerRsp :: Int,
    compilerK :: Int
  }

newCompiler :: Compiler
newCompiler = Compiler [] M.empty [] 0 0

nextK :: State Compiler Int
nextK = state $ \c -> let k = compilerK c in (k, c {compilerK = succ k})

pushLocals :: State Compiler ()
pushLocals = modify $ \c -> c {compilerLocals = M.empty : compilerLocals c}

popLocals :: State Compiler ()
popLocals = modify $ \c -> c {compilerLocals = tail $ compilerLocals c}

getRsp :: State Compiler Int
getRsp = state $ \c -> (compilerRsp c, c)

setRsp :: Int -> State Compiler ()
setRsp rsp = modify $ \c -> c {compilerRsp = rsp}

setInst :: Inst -> State Compiler ()
setInst inst = modify $ \c -> c {compilerInsts = inst : compilerInsts c}

setInsts :: [Inst] -> State Compiler ()
setInsts = mapM_ setInst

quadWord :: Int
quadWord = 8

rax :: Op
rax = OpReg RegRax

addRsp :: Int -> State Compiler ()
addRsp n = modify $ \c -> c {compilerRsp = compilerRsp c + n}

subRsp :: Int -> State Compiler ()
subRsp n = modify $ \c -> c {compilerRsp = compilerRsp c - n}

setInstPush :: Op -> State Compiler ()
setInstPush op = do
  setInst $ InstPush op
  subRsp quadWord

setInstPop :: Op -> State Compiler ()
setInstPop op = do
  setInst $ InstPop op
  addRsp quadWord

getLocal :: String -> State Compiler Int
getLocal var =
  state $ \c -> (head $ mapMaybe (M.lookup var) $ compilerLocals c, c)

setLocal :: String -> State Compiler ()
setLocal var = do
  rsp <- getRsp
  modify $ \c ->
    case compilerLocals c of
      (locals : rest) -> c {compilerLocals = M.insert var rsp locals : rest}
      _ -> undefined

compileString :: String -> State Compiler String
compileString str = do
  maybeLabel <- state $ \c -> (M.lookup str $ compilerStrings c, c)
  case maybeLabel of
    Just label -> return label
    Nothing -> do
      k <- nextK
      let label = printf "_str_%d_" k :: String
      state $ \c ->
        (label, c {compilerStrings = M.insert str label $ compilerStrings c})

intoAsmString :: String -> String
intoAsmString = intercalate "," . map show . (++ [0]) . map ord

compileCallArgs :: [Expr] -> [Reg] -> State Compiler ()
compileCallArgs [] _ = return ()
compileCallArgs _ [] = undefined
compileCallArgs (arg : args) (reg : regs) = do
  compileExpr arg
  compileCallArgs args regs
  setInstPop $ OpReg reg

argRegs :: [Reg]
argRegs = [RegRdi, RegRsi, RegRdx, RegRcx, RegR8, RegR9]

compileBinOp :: Expr -> Expr -> Reg -> Reg -> State Compiler ()
compileBinOp exprLeft exprRight regLeft regRight = do
  compileExpr exprLeft
  compileExpr exprRight
  setInstPop $ OpReg regRight
  setInstPop $ OpReg regLeft

compileSpawnArg :: Expr -> State Compiler ()
compileSpawnArg arg = do
  setInst $
    InstMov (OpReg RegRdi) $ OpAddrOffset $ AddrOffset (AddrReg RegRsp) 0
  compileExpr arg
  setInstPop $ OpReg RegRsi
  setInst $ InstCall $ OpLabel "thread_push_stack"

intoYieldLabel :: String -> String
intoYieldLabel = printf "_%s_yield_"

compileYield :: String -> State Compiler ()
compileYield label = do
  setInsts
    [ InstMov rax $ OpAddrOffset $ AddrOffset (AddrLabel "THREAD") 0,
      InstMov (OpAddrOffset $ AddrOffset (AddrReg RegRax) quadWord) (OpReg RegRsp),
      InstMov (OpAddrOffset $ AddrOffset (AddrReg RegRax) $ quadWord * 2) $
        OpReg RegRbp,
      InstMov (OpAddrOffset $ AddrOffset (AddrReg RegRax) 0) $
        OpLabel yieldLabel,
      InstMov (OpReg RegRsp) $
        OpAddrOffset $ AddrOffset (AddrLabel "SCHED_RSP") 0,
      InstMov (OpReg RegRbp) $
        OpAddrOffset $ AddrOffset (AddrLabel "SCHED_RBP") 0,
      InstJmp $ OpLabel "scheduler",
      InstLabel yieldLabel,
      InstMov rax $ OpAddrOffset $ AddrOffset (AddrLabel "THREAD") 0,
      InstMov (OpReg RegRsp) $
        OpAddrOffset $ AddrOffset (AddrReg RegRax) quadWord,
      InstMov (OpReg RegRbp) $
        OpAddrOffset $ AddrOffset (AddrReg RegRax) $ quadWord * 2
    ]
  where
    yieldLabel = intoYieldLabel label

compileExpr :: Expr -> State Compiler ()
compileExpr (ExprInt int) = setInstPush $ OpImm int
compileExpr (ExprVar var) = do
  rsp <- getRsp
  rspPrev <- getLocal var
  setInstPush $ OpAddrOffset $ AddrOffset (AddrReg RegRsp) $ rspPrev - rsp
compileExpr (ExprStr str) = do
  label <- compileString str
  setInstPush $ OpLabel label
compileExpr (ExprCall "-" [exprLeft, exprRight]) = do
  compileBinOp exprLeft exprRight RegR10 RegR11
  setInst $ InstSub (OpReg RegR10) (OpReg RegR11)
  setInstPush $ OpReg RegR10
compileExpr (ExprCall "-" _) = undefined
compileExpr (ExprCall "spawn" (ExprVar func : args)) = do
  setInsts
    [ InstMov (OpReg RegRdi) (OpLabel $ intoYieldLabel func),
      InstCall (OpLabel "thread_new")
    ]
  setInstPush rax
  mapM_ compileSpawnArg args
compileExpr (ExprCall "printf" args) = do
  compileCallArgs args argRegs
  setInsts [InstXor rax rax, InstCall $ OpLabel "printf"]
  setInstPush rax
compileExpr (ExprCall func args) = do
  compileCallArgs args argRegs
  setInst $ InstCall $ OpLabel func
  setInstPush rax
compileExpr
  (ExprIfElse (ExprCall "=" [exprLeft, exprRight]) scopeTrue scopeFalse) = do
    labelElse <- printf "_else_%d_" <$> nextK
    labelEnd <- printf "_end_%d_" <$> nextK
    compileBinOp exprLeft exprRight RegR10 RegR11
    setInsts
      [ InstCmp (OpReg RegR10) (OpReg RegR11),
        InstJnz $ OpLabel labelElse
      ]
    compileScope scopeTrue
    addRsp quadWord
    setInsts [InstJmp $ OpLabel labelEnd, InstLabel labelElse]
    compileScope scopeFalse
    setInst $ InstLabel labelEnd
compileExpr (ExprIfElse {}) = undefined

dropStack :: Int -> Int -> State Compiler ()
dropStack rspPre rspPost
  | rspPre == rspPost = return ()
  | otherwise = do
    setInst $ InstAdd (OpReg RegRsp) $ OpImm $ rspPre - rspPost
    setRsp rspPre

compileScope :: Scope -> State Compiler ()
compileScope (Scope [] expr) = compileExpr expr
compileScope (Scope stmts expr) = do
  pushLocals
  rspPre <- getRsp
  mapM_ compileStmt stmts
  rspPost <- getRsp
  compileExpr expr
  setInstPop rax
  dropStack rspPre rspPost
  setInstPush rax
  popLocals

compileStmt :: Stmt -> State Compiler ()
compileStmt (StmtEffect expr) = do
  compileExpr expr
  setInst $ InstAdd (OpReg RegRsp) (OpImm quadWord)
  addRsp quadWord
compileStmt (StmtLet var scope) = do
  compileScope scope
  setLocal var

compileFuncArgs :: [String] -> [Reg] -> State Compiler ()
compileFuncArgs [] _ = return ()
compileFuncArgs _ [] = undefined
compileFuncArgs (arg : args) (reg : regs) = do
  setInstPush $ OpReg reg
  setLocal arg
  compileFuncArgs args regs

compileFunc :: Func -> State Compiler ()
compileFunc (Func label args scope) = do
  pushLocals
  rspPre <- getRsp
  setInst $ InstLabel label
  compileFuncArgs args argRegs
  compileYield label
  compileScope scope
  setInstPop rax
  rspPost <- getRsp
  dropStack rspPre rspPost
  setInst InstRet
  popLocals

optimizePushPop :: [Inst] -> [Inst]
optimizePushPop [] = []
optimizePushPop (InstPush opPush : InstPop opPop : insts)
  | opPush == opPop = optimizePushPop insts
  | otherwise = InstMov opPop opPush : optimizePushPop insts
optimizePushPop (InstPush _ : InstAdd (OpReg RegRsp) (OpImm 8) : insts) =
  optimizePushPop insts
optimizePushPop (inst : insts) = inst : optimizePushPop insts

optimize :: State Compiler ()
optimize = modify $ \c -> c {compilerInsts = optimizePushPop $ compilerInsts c}

compile :: [Func] -> State Compiler ()
compile funcs = do
  mapM_ compileFunc funcs
  modify $ \c -> c {compilerInsts = reverse $ compilerInsts c}
  optimize
