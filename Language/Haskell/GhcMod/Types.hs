module Language.Haskell.GhcMod.Types where

import Data.List (intercalate)

-- | Output style.
data OutputStyle = LispStyle  -- ^ S expression style.
                 | PlainStyle -- ^ Plain textstyle.

-- | The type for line separator. Historically, a Null string is used.
newtype LineSeparator = LineSeparator String

data Options = Options {
    outputStyle   :: OutputStyle
  , hlintOpts     :: [String]
  , ghcOpts       :: [GHCOption]
  -- | If 'True', 'browse' also returns operators.
  , operators     :: Bool
  -- | If 'True', 'browse' also returns types.
  , detailed      :: Bool
  -- | If 'True', 'browse' will return fully qualified name
  , qualified     :: Bool
  -- | Line separator string.
  , lineSeparator :: LineSeparator
  }

-- | A default 'Options'.
defaultOptions :: Options
defaultOptions = Options {
    outputStyle   = PlainStyle
  , hlintOpts     = []
  , ghcOpts       = []
  , operators     = False
  , detailed      = False
  , qualified     = False
  , lineSeparator = LineSeparator "\0"
  }

----------------------------------------------------------------

-- | The environment where this library is used.
data Cradle = Cradle {
  -- | The directory where this library is executed.
    cradleCurrentDir :: FilePath
  -- | The project root directory.
  , cradleRootDir    :: FilePath
  -- | The file name of the found cabal file.
  , cradleCabalFile  :: Maybe FilePath
  -- | Package database stack
  , cradlePkgDbStack  :: [GhcPkgDb]
  } deriving (Eq, Show)

----------------------------------------------------------------

-- | GHC package database flags.
data GhcPkgDb = GlobalDb | UserDb | PackageDb String deriving (Eq, Show)

-- | A single GHC command line option.
type GHCOption  = String

-- | An include directory for modules.
type IncludeDir = FilePath

-- | A package name.
type PackageBaseName = String

-- | A package version.
type PackageVersion  = String

-- | A package id.
type PackageId  = String

-- | A package's name, verson and id.
type Package    = (PackageBaseName, PackageVersion, PackageId)

pkgName :: Package -> PackageBaseName
pkgName (n,_,_) = n

pkgVer :: Package -> PackageVersion
pkgVer (_,v,_) = v

pkgId :: Package -> PackageId
pkgId (_,_,i) = i

showPkg :: Package -> String
showPkg (n,v,_) = intercalate "-" [n,v]

showPkgId :: Package -> String
showPkgId (n,v,i) = intercalate "-" [n,v,i]

-- | Haskell expression.
type Expression = String

-- | Module name.
type ModuleString = String

-- | Option information for GHC
data CompilerOptions = CompilerOptions {
    ghcOptions  :: [GHCOption]  -- ^ Command line options
  , includeDirs :: [IncludeDir] -- ^ Include directories for modules
  , depPackages :: [Package]    -- ^ Dependent package names
  } deriving (Eq, Show)
