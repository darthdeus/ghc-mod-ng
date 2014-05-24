{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.GhcMod.Evaluator (
  evaluateRequest
) where

import Config (cProjectVersion)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..))
import Control.Exception (Exception)
import Control.Applicative ((<$>))
import qualified Control.Exception as E
import qualified System.Console.GetOpt as O
import System.Directory (doesFileExist)
import Data.Version (showVersion)

import Paths_ghc_mod
import Language.Haskell.GhcMod.Boot
import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Cradle (findCradle)
import Language.Haskell.GhcMod.Check
import Language.Haskell.GhcMod.Debug
import Language.Haskell.GhcMod.Flag
import Language.Haskell.GhcMod.Find
import Language.Haskell.GhcMod.Info
import Language.Haskell.GhcMod.Lang
import Language.Haskell.GhcMod.Lint
import Language.Haskell.GhcMod.List
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.PkgDoc
import Data.Typeable (Typeable)

----------------------------------------------------------------

data GHCModError = SafeList
                 | TooManyArguments String
                 | NoSuchCommand String
                 | CmdArg [String]
                 | FileNotExist String deriving (Show, Typeable)

instance Exception GHCModError

----------------------------------------------------------------

progVersion :: String
progVersion = "ghc-mod version " ++ showVersion version ++ " compiled by GHC " ++ cProjectVersion ++ "\n"

----------------------------------------------------------------

ghcOptHelp :: String
ghcOptHelp = " [-g GHC_opt1 -g GHC_opt2 ...] "

----------------------------------------------------------------

usage :: String
usage =    progVersion
        ++ "Usage:\n"
        ++ "\t ghc-mod list" ++ ghcOptHelp ++ "[-l] [-d]\n"
        ++ "\t ghc-mod lang [-l]\n"
        ++ "\t ghc-mod flag [-l]\n"
        ++ "\t ghc-mod browse" ++ ghcOptHelp ++ "[-l] [-o] [-d] [-q] [<package>:]<module> [[<package>:]<module> ...]\n"
        ++ "\t ghc-mod check" ++ ghcOptHelp ++ "<HaskellFiles...>\n"
        ++ "\t ghc-mod expand" ++ ghcOptHelp ++ "<HaskellFiles...>\n"
        ++ "\t ghc-mod debug" ++ ghcOptHelp ++ "\n"
        ++ "\t ghc-mod info" ++ ghcOptHelp ++ "<HaskellFile> <module> <expression>\n"
        ++ "\t ghc-mod type" ++ ghcOptHelp ++ "<HaskellFile> <module> <line-no> <column-no>\n"
        ++ "\t ghc-mod find <symbol>\n"
        ++ "\t ghc-mod lint [-h opt] <HaskellFile>\n"
        ++ "\t ghc-mod root\n"
        ++ "\t ghc-mod doc <module>\n"
        ++ "\t ghc-mod boot\n"
        ++ "\t ghc-mod version\n"
        ++ "\t ghc-mod help\n"
        ++ "\n"
        ++ "<module> for \"info\" and \"type\" is not used, anything is OK.\n"
        ++ "It is necessary to maintain backward compatibility.\n"

----------------------------------------------------------------

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "l" ["tolisp"]
            (NoArg (\opts -> opts { outputStyle = LispStyle }))
            "print as a list of Lisp"
          , Option "h" ["hlintOpt"]
            (ReqArg (\h opts -> opts { hlintOpts = h : hlintOpts opts }) "hlintOpt")
            "hlint options"
          , Option "g" ["ghcOpt"]
            (ReqArg (\g opts -> opts { ghcOpts = g : ghcOpts opts }) "ghcOpt")
            "GHC options"
          , Option "o" ["operators"]
            (NoArg (\opts -> opts { operators = True }))
            "print operators, too"
          , Option "d" ["detailed"]
            (NoArg (\opts -> opts { detailed = True }))
            "print detailed info"
          , Option "q" ["qualified"]
            (NoArg (\opts -> opts { qualified = True }))
            "show qualified names"
          , Option "b" ["boundary"]
            (ReqArg (\s opts -> opts { lineSeparator = LineSeparator s }) "sep")
            "specify line separator (default is Nul string)"
          ]

----------------------------------------------------------------

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case O.getOpt Permute spec argv of
        (o,n,[]  ) -> (foldr id defaultOptions o, n)
        (_,_,errs) -> E.throw (CmdArg errs)

----------------------------------------------------------------

evaluateRequest :: [String] -> IO String
evaluateRequest args = do
    let (opt,cmdArg) = parseArgs argspec args
    cradle <- findCradle

    let cmdArg0 = cmdArg !. 0
        cmdArg1 = cmdArg !. 1
        cmdArg3 = cmdArg !. 3
        cmdArg4 = cmdArg !. 4
        remainingArgs = tail cmdArg
        nArgs n f = if length remainingArgs == n
                        then f
                        else E.throw (TooManyArguments cmdArg0)
    case cmdArg0 of
      "list"    -> listModules opt cradle
      "lang"    -> listLanguages opt
      "flag"    -> listFlags opt
      "browse"  -> runGhcMod opt $ concat <$> mapM browse remainingArgs
      "check"   -> runGhcMod opt $ checkSyntax remainingArgs
      "expand"  -> runGhcMod opt $ expandTemplate remainingArgs
      "debug"   -> debugInfo opt cradle
      "info"    -> nArgs 3 infoExpr opt cradle cmdArg1 cmdArg3
      "type"    -> nArgs 4 $ typeExpr opt cradle cmdArg1 (read cmdArg3) (read cmdArg4)
      "find"    -> runGhcMod opt $ nArgs 1 $ findSymbol cmdArg1
      "lint"    -> nArgs 1 withFile (lintSyntax opt) cmdArg1
      "root"    -> rootInfo opt cradle
      "doc"     -> nArgs 1 $ packageDoc opt cradle cmdArg1
      "boot"    -> bootInfo opt
      "version" -> return progVersion
      "help"    -> return $ O.usageInfo usage argspec
      cmd       -> E.throw (NoSuchCommand cmd)

  where
    xs !. idx
      | length xs <= idx = E.throw SafeList
      | otherwise = xs !! idx
    withFile cmd file = do
        exist <- doesFileExist file
        if exist
            then cmd file
            else E.throw (FileNotExist file)
