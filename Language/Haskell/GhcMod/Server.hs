{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.GhcMod.Server where

import Control.Monad
import System.FilePath.Posix
import System.Posix.Files
import System.Posix.Process
import System.IO

import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network.Unix
import qualified Data.Conduit.List as CL

-- import Data.ByteString.Lazy
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

import Language.Haskell.GhcMod.Evaluator

runServer :: IO ()
runServer = runResourceT $ do
    void $ register $ removeLink "mod.sock"
    let settings = (serverSettings "mod.sock")
    liftIO $ runUnixServer settings (\ad -> (appSource ad) $$ CL.mapM evaluateThingy =$ (appSink ad))

evaluateThingy :: BS.ByteString -> IO BS.ByteString
evaluateThingy = fmap BS.pack . evaluateRequest . words . BS.unpack

socketPath :: FilePath -> FilePath
socketPath path = path </> "ghc-mod.sock"

isServerRunning :: FilePath -> IO Bool
isServerRunning path = fileExist $ socketPath path
