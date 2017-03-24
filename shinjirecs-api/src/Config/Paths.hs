{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.Paths where
import Config.Class(ConfigClass)
import System.Directory(getCurrentDirectory)
import System.FilePath.Posix((</>),takeDirectory) -- filepath
import System.Argv0(getArgv0)

data PathsConfig = PathsConfig {
  baseDir    :: FilePath,
  privateDir :: FilePath,
  commandDir :: FilePath,
  videoFilesDir :: FilePath
  } deriving(Show)

defaultPathsConfig :: IO PathsConfig
defaultPathsConfig = do
  curr' <- getCurrentDirectory
  baseDir' <- return . (curr' </>) . takeDirectory . show =<< getArgv0
  return PathsConfig {
    baseDir    = baseDir',
    privateDir = "private",
    commandDir = "private/commands",
    videoFilesDir = "private/videofiles"
  }
