{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ollama_holes_plugin (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1,6,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/nathanbloomfield/code/haskell-projects/harness/.stack-work/install/aarch64-osx/f178fe8f750335115fce530bc356de4b64d800436eed158721b01031188ab511/9.10.3/bin"
libdir     = "/Users/nathanbloomfield/code/haskell-projects/harness/.stack-work/install/aarch64-osx/f178fe8f750335115fce530bc356de4b64d800436eed158721b01031188ab511/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c/ollama-holes-plugin-0.1.6.0-BQxCbEDyZFRIpwTT4zqgpQ"
dynlibdir  = "/Users/nathanbloomfield/code/haskell-projects/harness/.stack-work/install/aarch64-osx/f178fe8f750335115fce530bc356de4b64d800436eed158721b01031188ab511/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c"
datadir    = "/Users/nathanbloomfield/code/haskell-projects/harness/.stack-work/install/aarch64-osx/f178fe8f750335115fce530bc356de4b64d800436eed158721b01031188ab511/9.10.3/share/aarch64-osx-ghc-9.10.3-fe9c/ollama-holes-plugin-0.1.6.0"
libexecdir = "/Users/nathanbloomfield/code/haskell-projects/harness/.stack-work/install/aarch64-osx/f178fe8f750335115fce530bc356de4b64d800436eed158721b01031188ab511/9.10.3/libexec/aarch64-osx-ghc-9.10.3-fe9c/ollama-holes-plugin-0.1.6.0"
sysconfdir = "/Users/nathanbloomfield/code/haskell-projects/harness/.stack-work/install/aarch64-osx/f178fe8f750335115fce530bc356de4b64d800436eed158721b01031188ab511/9.10.3/etc"

getBinDir     = catchIO (getEnv "ollama_holes_plugin_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ollama_holes_plugin_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ollama_holes_plugin_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ollama_holes_plugin_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ollama_holes_plugin_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ollama_holes_plugin_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
