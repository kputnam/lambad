module Paths_lambad (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/kputnam/.cabal/bin"
libdir     = "/Users/kputnam/.cabal/lib/lambad-0.0.0/ghc-7.4.2"
datadir    = "/Users/kputnam/.cabal/share/lambad-0.0.0"
libexecdir = "/Users/kputnam/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "lambad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lambad_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lambad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lambad_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
