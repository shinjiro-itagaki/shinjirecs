{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.Paths where
import Config.Class(ConfigClass(..), readInclude, lookupInt, lookupInteger, lookupText, lookupString,lookupWord)
import Config.Env(Env(..))
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

instance ConfigClass PathsConfig where
  defaultConfig env = defaultPathsConfig
  objectToConfig obj dflt =  
    PathsConfig {
    baseDir       = baseDir       `or'` (lookupString "baseDir"),
    privateDir    = privateDir    `or'` (lookupString "privateDir"),
    commandDir    = commandDir    `or'` (lookupString "commandDir"),
    videoFilesDir = videoFilesDir `or'` (lookupString "videoFilesDir")
    }
    where
      mor' = mor dflt obj
      or'  = Config.Class.or dflt obj
