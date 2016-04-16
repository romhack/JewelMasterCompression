module Paths_jmArith (
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

bindir     = "H:\\Projects\\JewelMaster\\jmArith\\.stack-work\\install\\89b54721\\bin"
libdir     = "H:\\Projects\\JewelMaster\\jmArith\\.stack-work\\install\\89b54721\\lib\\x86_64-windows-ghc-7.10.3\\jmArith-0.1.0.0-GVM8j0yxhfqJ3cSyYORrr4"
datadir    = "H:\\Projects\\JewelMaster\\jmArith\\.stack-work\\install\\89b54721\\share\\x86_64-windows-ghc-7.10.3\\jmArith-0.1.0.0"
libexecdir = "H:\\Projects\\JewelMaster\\jmArith\\.stack-work\\install\\89b54721\\libexec"
sysconfdir = "H:\\Projects\\JewelMaster\\jmArith\\.stack-work\\install\\89b54721\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "jmArith_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "jmArith_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "jmArith_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "jmArith_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "jmArith_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
