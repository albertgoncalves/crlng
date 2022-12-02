import Compile (compile)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as B
import Parse (parse)
import System.Environment (getArgs)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  [pathIn, pathOut] <- getArgs
  funcs <- parse <$> readFile pathIn
  mapM_ (hPrint stderr) funcs
  B.writeFile pathOut $ toLazyByteString $ compile funcs
