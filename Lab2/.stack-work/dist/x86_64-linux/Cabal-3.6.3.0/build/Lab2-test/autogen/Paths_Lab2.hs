{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Lab2 (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/rust/haski/MIET_haskell/miet-haskell-course/Lab2/.stack-work/install/x86_64-linux/85a156748addcbbf2ae61aee9404218b2cf269ae4a7a82c70116ba2249eda493/9.2.5/bin"
libdir     = "/home/rust/haski/MIET_haskell/miet-haskell-course/Lab2/.stack-work/install/x86_64-linux/85a156748addcbbf2ae61aee9404218b2cf269ae4a7a82c70116ba2249eda493/9.2.5/lib/x86_64-linux-ghc-9.2.5/Lab2-0.1.0.0-FTj6oNcAmOg3yXnHVH4WBh-Lab2-test"
dynlibdir  = "/home/rust/haski/MIET_haskell/miet-haskell-course/Lab2/.stack-work/install/x86_64-linux/85a156748addcbbf2ae61aee9404218b2cf269ae4a7a82c70116ba2249eda493/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/rust/haski/MIET_haskell/miet-haskell-course/Lab2/.stack-work/install/x86_64-linux/85a156748addcbbf2ae61aee9404218b2cf269ae4a7a82c70116ba2249eda493/9.2.5/share/x86_64-linux-ghc-9.2.5/Lab2-0.1.0.0"
libexecdir = "/home/rust/haski/MIET_haskell/miet-haskell-course/Lab2/.stack-work/install/x86_64-linux/85a156748addcbbf2ae61aee9404218b2cf269ae4a7a82c70116ba2249eda493/9.2.5/libexec/x86_64-linux-ghc-9.2.5/Lab2-0.1.0.0"
sysconfdir = "/home/rust/haski/MIET_haskell/miet-haskell-course/Lab2/.stack-work/install/x86_64-linux/85a156748addcbbf2ae61aee9404218b2cf269ae4a7a82c70116ba2249eda493/9.2.5/etc"

getBinDir     = catchIO (getEnv "Lab2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Lab2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Lab2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Lab2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lab2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Lab2_sysconfdir") (\_ -> return sysconfdir)




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
