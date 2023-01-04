{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Compile where

import Ast (Expr (..), Func (..), Scope (..), Stmt (..))
import Control.Exception (assert)
import Control.Monad.State (State, execState, modify, state)
import Data.ByteString.Builder (Builder, charUtf8, stringUtf8)
import Data.Char (ord)
import Data.List (groupBy, intercalate)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Text.Printf (printf)

data Inst
  = InstPush Op
  | InstPop Op
  | InstMov Op Op
  | InstCall Op
  | InstJmp Op
  | InstJz Op
  | InstJnz Op
  | InstCmp Op Op
  | InstTest Op Op
  | InstXor Op Op
  | InstAdd Op Op
  | InstSub Op Op
  | InstIMul Op Op
  | InstIDiv Op
  | InstCqo
  | InstSets Op
  | InstCmovz Op Op
  | InstCmovns Op Op
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
  | RegR10b
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
  show (InstJz op) = printf "\tjz %s" $ show op
  show (InstJnz op) = printf "\tjnz %s" $ show op
  show (InstCmp opDst opSrc) = printf "\tcmp %s, %s" (show opDst) (show opSrc)
  show (InstTest opDst opSrc) =
    printf "\ttest %s, %s" (show opDst) (show opSrc)
  show (InstXor opDst opSrc) = printf "\txor %s, %s" (show opDst) (show opSrc)
  show (InstAdd opDst opSrc) = printf "\tadd %s, %s" (show opDst) (show opSrc)
  show (InstSub opDst opSrc) = printf "\tsub %s, %s" (show opDst) (show opSrc)
  show (InstIMul opDst opSrc) =
    printf "\timul %s, %s" (show opDst) (show opSrc)
  show (InstIDiv op) = printf "\tidiv %s" $ show op
  show InstCqo = "\tcqo"
  show (InstSets op) = printf "\tsets %s" $ show op
  show (InstCmovz opDst opSrc) =
    printf "\tcmovz %s, %s" (show opDst) (show opSrc)
  show (InstCmovns opDst opSrc) =
    printf "\tcmovns %s, %s" (show opDst) (show opSrc)
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
  show RegR10b = "r10b"
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
    compilerSpawns :: M.Map String Int,
    compilerLabels :: S.Set String,
    compilerRsp :: Int,
    compilerK :: Int
  }

isLabel :: Inst -> Bool
isLabel (InstLabel _) = True
isLabel _ = False

newCompiler :: Compiler
newCompiler = Compiler [] M.empty [] M.empty (S.singleton "main_thread") 0 0

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

getSpawns :: State Compiler (M.Map String Int)
getSpawns = state $ \c -> (compilerSpawns c, c)

setSpawn :: String -> Int -> State Compiler ()
setSpawn func lenArgs = do
  spawns <- getSpawns
  if M.member func spawns
    then return ()
    else modify $
      \c -> c {compilerSpawns = M.insert func lenArgs $ compilerSpawns c}

getLabels :: State Compiler (S.Set String)
getLabels = state $ \c -> (compilerLabels c, c)

setLabel :: String -> State Compiler ()
setLabel label = do
  labels <- getLabels
  if S.member label labels
    then return ()
    else modify $ \c -> c {compilerLabels = S.insert label $ compilerLabels c}

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

setInstPop :: Maybe Op -> State Compiler ()
setInstPop Nothing = do
  setInst $ InstAdd (OpReg RegRsp) (OpImm quadWord)
  addRsp quadWord
setInstPop (Just op) = do
  setInst $ InstPop op
  addRsp quadWord

resetStack :: Int -> Int -> State Compiler ()
resetStack rspPre rspPost
  | rspPre == rspPost = return ()
  | otherwise = do
      setInst $ InstAdd (OpReg RegRsp) $ OpImm $ rspPre - rspPost
      setRsp rspPre

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

intoOpLabel :: String -> State Compiler Op
intoOpLabel label = do
  setLabel label
  return $ OpLabel label

compileCallArgs :: [Expr] -> [Reg] -> State Compiler ()
compileCallArgs [] _ = return ()
compileCallArgs _ [] = undefined
compileCallArgs (arg : args) (reg : regs) = do
  compileExpr arg
  compileCallArgs args regs
  setInstPop $ Just $ OpReg reg

argRegs :: [Reg]
argRegs = [RegRdi, RegRsi, RegRdx, RegRcx, RegR8, RegR9]

compileBinOpArgs :: Expr -> Expr -> Reg -> Reg -> State Compiler ()
compileBinOpArgs exprLeft exprRight regLeft regRight = do
  compileExpr exprLeft
  compileExpr exprRight
  setInstPop $ Just $ OpReg regRight
  setInstPop $ Just $ OpReg regLeft

compileSpawnArg :: Expr -> State Compiler ()
compileSpawnArg arg = do
  setInst $
    InstMov (OpReg RegRdi) $
      OpAddrOffset $
        AddrOffset (AddrReg RegRsp) 0
  compileExpr arg
  setInstPop $ Just $ OpReg RegRsi
  setInst . InstCall =<< intoOpLabel "thread_push_stack"

intoSpawnLabel :: String -> String
intoSpawnLabel = printf "_%s_thread_"

intoYieldLabel :: String -> String
intoYieldLabel = printf "_%s_yield_"

setInstsJumpScheduler :: State Compiler ()
setInstsJumpScheduler = do
  setInsts
    [ InstMov (OpReg RegRsp) $
        OpAddrOffset $
          AddrOffset (AddrLabel "SCHED_RSP") 0,
      InstMov (OpReg RegRbp) $
        OpAddrOffset $
          AddrOffset (AddrLabel "SCHED_RBP") 0
    ]
  setInst . InstJmp =<< intoOpLabel "scheduler"

compileYield :: String -> State Compiler ()
compileYield label = do
  setInsts
    [ InstMov rax $ OpAddrOffset $ AddrOffset (AddrLabel "THREAD") 0,
      InstMov
        (OpAddrOffset $ AddrOffset (AddrReg RegRax) quadWord)
        (OpReg RegRsp),
      InstMov (OpAddrOffset $ AddrOffset (AddrReg RegRax) $ quadWord * 2) $
        OpReg RegRbp
    ]
  setInst . InstMov (OpAddrOffset $ AddrOffset (AddrReg RegRax) 0)
    =<< intoOpLabel yieldLabel
  setInstsJumpScheduler
  setInsts
    [ InstLabel yieldLabel,
      InstMov rax $ OpAddrOffset $ AddrOffset (AddrLabel "THREAD") 0,
      InstMov (OpReg RegRsp) $
        OpAddrOffset $
          AddrOffset (AddrReg RegRax) quadWord,
      InstMov (OpReg RegRbp) $
        OpAddrOffset $
          AddrOffset (AddrReg RegRax) $
            quadWord * 2
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
  setInstPush =<< intoOpLabel label
compileExpr (ExprCall _ "+" [exprLeft, exprRight]) = do
  compileBinOpArgs exprLeft exprRight RegR10 RegR11
  setInst $ InstAdd (OpReg RegR10) (OpReg RegR11)
  setInstPush $ OpReg RegR10
compileExpr (ExprCall _ "-" [exprLeft, exprRight]) = do
  compileBinOpArgs exprLeft exprRight RegR10 RegR11
  setInst $ InstSub (OpReg RegR10) (OpReg RegR11)
  setInstPush $ OpReg RegR10
compileExpr (ExprCall _ "*" [exprLeft, exprRight]) = do
  compileBinOpArgs exprLeft exprRight RegR10 RegR11
  setInst $ InstIMul (OpReg RegR10) (OpReg RegR11)
  setInstPush $ OpReg RegR10
compileExpr (ExprCall _ "/" [exprLeft, exprRight]) = do
  compileExpr exprLeft
  compileExpr exprRight
  setInst $ InstXor (OpReg RegR10) (OpReg RegR10)
  setInstPop $ Just $ OpReg RegR11
  setInstPop $ Just rax
  setInsts
    [ InstCqo,
      InstIDiv $ OpReg RegR11,
      InstTest rax rax,
      InstSets $ OpReg RegR10b,
      InstTest (OpReg RegRdx) (OpReg RegRdx),
      InstCmovz (OpReg RegR10) (OpReg RegRdx),
      InstSub rax (OpReg RegR10)
    ]
  setInstPush rax
compileExpr (ExprCall _ "%" [exprLeft, exprRight]) = do
  compileExpr exprLeft
  compileExpr exprRight
  setInst $ InstXor (OpReg RegRcx) (OpReg RegRcx)
  setInstPop $ Just $ OpReg RegR11
  setInstPop $ Just rax
  setInsts
    [ InstMov (OpReg RegR10) (OpReg RegR11),
      InstCqo,
      InstIDiv $ OpReg RegR11,
      InstTest rax rax,
      InstCmovns (OpReg RegR10) (OpReg RegRcx),
      InstTest (OpReg RegRdx) (OpReg RegRdx),
      InstCmovz (OpReg RegR10) (OpReg RegRcx),
      InstAdd (OpReg RegRdx) (OpReg RegR10)
    ]
  setInstPush $ OpReg RegRdx
compileExpr (ExprCall _ "self" _) =
  setInstPush $ OpAddrOffset $ AddrOffset (AddrLabel "THREAD") 0
compileExpr (ExprCall _ "spawn" (ExprVar func : args)) = do
  setSpawn func (length args)
  setInst . InstMov (OpReg RegRdi)
    =<< intoOpLabel (intoYieldLabel $ intoSpawnLabel func)
  setInst . InstCall =<< intoOpLabel "thread_new"
  setInstPush rax
  mapM_ compileSpawnArg args
  compileYield . printf "spawn_%d" =<< nextK
compileExpr (ExprCall _ "printf" args) = do
  compileCallArgs args argRegs
  setInst $ InstXor rax rax
  setInst . InstCall =<< intoOpLabel "printf"
  setInstPush rax
compileExpr (ExprCall _ "kill" []) = do
  setInst $
    InstMov (OpReg RegRdi) $
      OpAddrOffset $
        AddrOffset (AddrLabel "THREAD") 0
  setInst . InstCall =<< intoOpLabel "thread_kill"
  setInstsJumpScheduler
  setInstPush $ OpImm 0
compileExpr (ExprCall False label args) = do
  compileCallArgs args argRegs
  setInst . InstCall =<< intoOpLabel label
  setInstPush rax
compileExpr (ExprCall True label args) = do
  setInst . InstCall =<< intoOpLabel "call_pop"
  compileCallArgs args argRegs
  setInst . InstAdd (OpReg RegRsp) . OpImm . negate =<< getRsp
  setInst . InstJmp =<< intoOpLabel label
  setInstPush $ OpImm 0
compileExpr (ExprIfElse cond scopeTrue scopeFalse) = do
  labelElse <- printf "_else_%d_" <$> nextK
  labelEnd <- printf "_end_%d_" <$> nextK
  compileCondition labelElse cond
  rspPre <- getRsp
  compileScope scopeTrue
  setRsp rspPre
  setInst . InstJmp =<< intoOpLabel labelEnd
  setInst $ InstLabel labelElse
  compileScope scopeFalse
  setInst $ InstLabel labelEnd

compileCondition :: String -> Expr -> State Compiler ()
compileCondition label (ExprCall _ "=" [exprLeft, exprRight]) = do
  compileBinOpArgs exprLeft exprRight RegR10 RegR11
  setInst $ InstCmp (OpReg RegR10) (OpReg RegR11)
  setInst . InstJnz =<< intoOpLabel label
compileCondition label expr = do
  compileExpr expr
  setInstPop $ Just rax
  setInst $ InstTest rax rax
  setInst . InstJz =<< intoOpLabel label

compileScope :: Scope -> State Compiler ()
compileScope (Scope [] expr) = compileExpr expr
compileScope (Scope body expr) = do
  pushLocals
  rspPre <- getRsp
  mapM_ compileStmt body
  rspPost <- getRsp
  compileExpr expr
  setInstPop $ Just rax
  resetStack rspPre rspPost
  setInstPush rax
  popLocals

compileStmt :: Stmt -> State Compiler ()
compileStmt (StmtEffect expr) = do
  compileExpr expr
  setInstPop Nothing
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
  localsPre <- state $ \c -> (compilerLocals c, c)
  assert (null localsPre) $ return ()
  pushLocals
  rspPre <- getRsp
  assert (rspPre == 0) $ return ()
  setInst $ InstLabel label
  compileFuncArgs args argRegs
  setInst . InstCall =<< intoOpLabel "stack_overflow"
  compileYield label
  setInst . InstMov (OpReg RegRdi) =<< intoOpLabel =<< compileString label
  setInst . InstCall =<< intoOpLabel "call_push"
  compileScope scope
  setInst . InstCall =<< intoOpLabel "call_pop"
  setInstPop $ Just rax
  rspPost <- getRsp
  resetStack rspPre rspPost
  setInst InstRet
  popLocals

spawnToFunc :: String -> Int -> Func
spawnToFunc func lenArgs =
  Func (intoSpawnLabel func) args $
    Scope
      [StmtEffect $ ExprCall False func $ map ExprVar args]
      (ExprCall False "kill" [])
  where
    args = map (: []) $ take lenArgs ['a' ..]

optimizePushPop :: [Inst] -> [Inst]
optimizePushPop [] = []
optimizePushPop (InstPush opPush : InstPop opPop : insts)
  | opPush == opPop = optimizePushPop insts
  | otherwise = InstMov opPop opPush : optimizePushPop insts
optimizePushPop (InstPush _ : InstAdd (OpReg RegRsp) (OpImm 8) : insts) =
  optimizePushPop insts
optimizePushPop (inst : insts) = inst : optimizePushPop insts

optimizeUnreachable :: [Inst] -> [Inst]
optimizeUnreachable [] = []
optimizeUnreachable (jump@(InstJmp _) : insts@(InstLabel _ : _)) =
  jump : optimizeUnreachable insts
optimizeUnreachable (jump@(InstJmp _) : _ : insts) =
  optimizeUnreachable $ jump : insts
optimizeUnreachable (InstRet : insts@(InstLabel _ : _)) =
  InstRet : optimizeUnreachable insts
optimizeUnreachable (InstRet : _ : insts) =
  optimizeUnreachable $ InstRet : insts
optimizeUnreachable (inst : insts) = inst : optimizeUnreachable insts

optimizeDeadCode :: S.Set String -> [Inst] -> [Inst]
optimizeDeadCode labels insts =
  concat
    $ filter
      ( \case
          (InstLabel label : _) -> S.member label labels
          _ -> undefined
      )
    $ groupBy (const $ not . isLabel) insts

optimize :: S.Set String -> [Inst] -> [Inst]
optimize labels =
  optimizeDeadCode labels . optimizePushPop . optimizeUnreachable

compileFuncs :: [Func] -> State Compiler ()
compileFuncs funcs = do
  mapM_ compileFunc funcs
  mapM_ (compileFunc . uncurry spawnToFunc) . M.toList =<< getSpawns
  labels <- getLabels
  modify $
    \c -> c {compilerInsts = optimize labels $ reverse $ compilerInsts c}

compile :: [Func] -> Builder
compile funcs =
  if null (compilerStrings program)
    then header <> insts
    else header <> strings <> insts
  where
    program = execState (compileFuncs funcs) newCompiler
    header =
      foldMap
        (<> charUtf8 '\n')
        [ "format ELF64",
          "public main_thread",
          "extrn printf",
          "extrn THREAD",
          "extrn SCHED_RSP",
          "extrn SCHED_RBP",
          "extrn scheduler",
          "extrn panic",
          "extrn receive",
          "extrn send",
          "extrn stack_overflow",
          "extrn thread_new",
          "extrn thread_kill",
          "extrn thread_push_stack",
          "extrn channel_new",
          "extrn channel_ready",
          "extrn call_push",
          "extrn call_pop"
        ]
    strings =
      "section '.rodata'\n"
        <> foldMap
          (stringUtf8 . uncurry (flip (printf "\t%s db %s\n") . intoAsmString))
          (M.toList $ compilerStrings program)
    insts =
      "section '.text' executable\n"
        <> foldMap
          (stringUtf8 . printf "\t%s\n" . show)
          (compilerInsts program)
