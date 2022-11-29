import Ast (Expr (..), Func (..), Scope (..), Stmt (..))
import Compile (compile)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)

main :: IO ()
main = do
  mapM_ print funcs
  path <- head <$> getArgs
  B.writeFile path $ toLazyByteString $ compile funcs
  where
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
