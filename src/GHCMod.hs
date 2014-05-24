module Main where

import Control.Exception (Handler(..), ErrorCall(..))
import qualified Control.Exception as E
import qualified System.Console.GetOpt as O
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stdout, stderr, hSetEncoding, utf8)

import Language.Haskell.GhcMod

----------------------------------------------------------------

main :: IO ()
main = flip E.catches handlers $ do
-- #if __GLASGOW_HASKELL__ >= 611
    hSetEncoding stdout utf8
-- #endif
    getArgs >>= evaluateRequest >>= putStrLn

  where
    handlers = [Handler (handleThenExit handler1), Handler (handleThenExit handler2)]
    handleThenExit handler e = handler e >> exitFailure

    handler1 :: ErrorCall -> IO ()
    handler1 = print -- for debug

    handler2 :: GHCModError -> IO ()
    handler2 SafeList = printUsage
    handler2 (TooManyArguments cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Too many arguments"
        printUsage
    handler2 (NoSuchCommand cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
        printUsage
    handler2 (CmdArg errs) = do
        mapM_ (hPutStr stderr) errs
        printUsage
    handler2 (FileNotExist file) = do
        hPutStrLn stderr $ "\"" ++ file ++ "\" not found"
        printUsage

    printUsage = hPutStrLn stderr $ '\n' : O.usageInfo usage argspec
