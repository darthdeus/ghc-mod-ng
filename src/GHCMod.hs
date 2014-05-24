module Main where

-- import Control.Exception (Handler(..), ErrorCall(..))
-- import qualified Control.Exception as E
import System.Environment (getArgs)
-- import System.Exit (exitFailure)
-- import System.IO (hPutStr, hPutStrLn, stdout, stderr, hSetEncoding, utf8)
--
import Paths_ghc_mod
import Language.Haskell.GhcMod
import System.IO (hSetEncoding, utf8, stdout)

----------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    getArgs >>= evaluateRequest >>= putStrLn
