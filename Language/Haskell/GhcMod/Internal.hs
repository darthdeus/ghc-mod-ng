-- | Low level access to the ghc-mod library.

module Language.Haskell.GhcMod.Internal (
  -- * Types
    GHCOption
  , Package
  , PackageBaseName
  , PackageVersion
  , PackageId
  , IncludeDir
  , CompilerOptions(..)
  -- * Cabal API
  , parseCabalFile
  , getCompilerOptions
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  -- * IO
  , getSystemLibDir
  , getDynamicFlags
  -- * Initializing 'DynFlags'
  , initializeFlagsWithCradle
  -- * Targets
  , setTargetFiles
  -- * Logging
  , withLogger
  , setNoWaringFlags
  , setAllWaringFlags
  -- * 'Ghc' Choice
  , (||>)
  , goNext
  , runAnyOne
  -- * 'GhcMonad' Choice
  , (|||>)
  ) where

import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.GHCChoice
import Language.Haskell.GhcMod.Logger
import Language.Haskell.GhcMod.Types
