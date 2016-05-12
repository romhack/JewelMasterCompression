module Paths_jmRle (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "E:\\Projects\\JewelMaster\\jmRle\\.stack-work\\install\\03c0d273\\bin"
libdir     = "E:\\Projects\\JewelMaster\\jmRle\\.stack-work\\install\\03c0d273\\lib\\x86_64-windows-ghc-7.10.3\\jmRle-0.1.0.0-D6rkKOicAWZ1aqPJlDDyjo"
datadir    = "E:\\Projects\\JewelMaster\\jmRle\\.stack-work\\install\\03c0d273\\share\\x86_64-windows-ghc-7.10.3\\jmRle-0.1.0.0"
libexecdir = "E:\\Projects\\JewelMaster\\jmRle\\.stack-work\\install\\03c0d273\\libexec"
sysconfdir = "E:\\Projects\\JewelMaster\\jmRle\\.stack-work\\install\\03c0d273\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "jmRle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "jmRle_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "jmRle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "jmRle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "jmRle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
