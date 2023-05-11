{-# LANGUAGE CPP #-}

#define VERBOSE 1

import Compile (compile)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as B
import Parse (parse)
import System.Environment (getArgs)

#if VERBOSE
import System.IO (hPrint, stderr)
#endif

main :: IO ()
main = do
  [pathIn, pathOut] <- getArgs
  funcs <- parse <$> readFile pathIn
#if VERBOSE
  mapM_ (hPrint stderr) funcs
#endif
  B.writeFile pathOut $ toLazyByteString $ compile funcs
