{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ceph (
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
version = Version [0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\robertg\\OneDrive\\cephaland-master\\.stack-work\\install\\babb6a16\\bin"
libdir     = "C:\\Users\\robertg\\OneDrive\\cephaland-master\\.stack-work\\install\\babb6a16\\lib\\x86_64-windows-ghc-8.6.5\\ceph-0.1.0-65ZcyPelNgsCgeYxFuN7YF-ceph"
dynlibdir  = "C:\\Users\\robertg\\OneDrive\\cephaland-master\\.stack-work\\install\\babb6a16\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\robertg\\OneDrive\\cephaland-master\\.stack-work\\install\\babb6a16\\share\\x86_64-windows-ghc-8.6.5\\ceph-0.1.0"
libexecdir = "C:\\Users\\robertg\\OneDrive\\cephaland-master\\.stack-work\\install\\babb6a16\\libexec\\x86_64-windows-ghc-8.6.5\\ceph-0.1.0"
sysconfdir = "C:\\Users\\robertg\\OneDrive\\cephaland-master\\.stack-work\\install\\babb6a16\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ceph_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ceph_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ceph_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ceph_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ceph_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ceph_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
