import Ast (Expr (..), Func (..), Scope (..), Stmt (..))
import Compile (Compiler (..), compile, intoAsmString, newCompiler)
import Control.Monad.State (execState)
import Data.List (intercalate)
import Data.Map (toList)
import System.IO (hPrint, stderr)
import Text.Printf (printf)

main :: IO ()
main = do
  mapM_ (hPrint stderr) funcs
  putStrLn $
    intercalate
      "\n"
      [ "format ELF64",
        "public _main_thread_yield_",
        "extrn THREAD",
        "extrn SCHED_RSP",
        "extrn SCHED_RBP",
        "extrn scheduler",
        "extrn channel_new",
        "extrn receive",
        "extrn send",
        "extrn kill",
        "extrn thread_new",
        "extrn thread_push_stack",
        "extrn printf"
      ]
  putStrLn "section '.rodata'"
  mapM_
    (putStrLn . uncurry (flip (printf "\t%s db %s") . intoAsmString))
    $ toList $ compilerStrings program
  putStrLn "section '.text' executable"
  mapM_ (putStrLn . ('\t' :) . show) $ compilerInsts program
  where
    program = execState (compile funcs) newCompiler
    funcs =
      [ Func
          "ping_pong"
          ["in", "out", "done", "message"]
          ( Scope
              [ StmtLet
                  "n"
                  (Scope [] (ExprCall "receive" [ExprVar "in"]))
              ]
              ( ExprIfElse
                  (ExprCall "=" [ExprVar "n", ExprInt 0])
                  ( Scope
                      [ StmtEffect
                          (ExprCall "send" [ExprVar "done", ExprInt 0])
                      ]
                      (ExprCall "kill" [])
                  )
                  ( Scope
                      [ StmtEffect
                          ( ExprCall
                              "printf"
                              [ExprStr "%s\n", ExprVar "message"]
                          ),
                        StmtEffect
                          ( ExprCall
                              "send"
                              [ ExprVar "out",
                                ExprCall "-" [ExprVar "n", ExprInt 1]
                              ]
                          )
                      ]
                      ( ExprCall
                          "ping_pong"
                          [ ExprVar "in",
                            ExprVar "out",
                            ExprVar "done",
                            ExprVar "message"
                          ]
                      )
                  )
              )
          ),
        Func
          "main_thread"
          []
          ( Scope
              [ StmtLet "ping" (Scope [] (ExprCall "channel_new" [])),
                StmtLet "pong" (Scope [] (ExprCall "channel_new" [])),
                StmtLet "done" (Scope [] (ExprCall "channel_new" [])),
                StmtEffect
                  ( ExprCall
                      "spawn"
                      [ ExprVar "ping_pong",
                        ExprVar "ping",
                        ExprVar "pong",
                        ExprVar "done",
                        ExprStr " - ping -"
                      ]
                  ),
                StmtEffect
                  ( ExprCall
                      "spawn"
                      [ ExprVar "ping_pong",
                        ExprVar "pong",
                        ExprVar "ping",
                        ExprVar "done",
                        ExprStr " - pong -"
                      ]
                  ),
                StmtEffect (ExprCall "send" [ExprVar "ping", ExprInt 5]),
                StmtEffect (ExprCall "receive" [ExprVar "done"]),
                StmtEffect (ExprCall "printf" [ExprStr "Done!\n"])
              ]
              (ExprCall "kill" [])
          )
      ]
