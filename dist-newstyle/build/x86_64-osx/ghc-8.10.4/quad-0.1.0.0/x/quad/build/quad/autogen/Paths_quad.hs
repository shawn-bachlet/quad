{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_quad (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/shawnbachlet/.cabal/bin"
libdir     = "/Users/shawnbachlet/.cabal/lib/x86_64-osx-ghc-8.10.4/quad-0.1.0.0-inplace-quad"
dynlibdir  = "/Users/shawnbachlet/.cabal/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/shawnbachlet/.cabal/share/x86_64-osx-ghc-8.10.4/quad-0.1.0.0"
libexecdir = "/Users/shawnbachlet/.cabal/libexec/x86_64-osx-ghc-8.10.4/quad-0.1.0.0"
sysconfdir = "/Users/shawnbachlet/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quad_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "quad_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "quad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
