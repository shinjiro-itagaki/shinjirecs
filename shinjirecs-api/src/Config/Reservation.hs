{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.Reservation where
import Config.Class(ConfigClass(..), Env,readInclude, lookupInt, lookupInteger, lookupText, lookupString,lookupWord)

data ReservationCommandArg = ArgDevice | ArgChannel | ArgDurationSec | ArgDestFilePath deriving (Show)

data ReservationConfig = ReservationConfig {
  marginStart :: Word,
  scriptFilePath :: FilePath
  } deriving (Show)

scriptArgs = [ArgDevice,ArgChannel,ArgDurationSec,ArgDestFilePath]

defaultReservationConfig :: ReservationConfig
defaultReservationConfig = ReservationConfig {
    marginStart = 3,
    scriptFilePath = "recpt1.sh"
    }

instance ConfigClass ReservationConfig where
  defaultConfig env = return defaultReservationConfig
  objectToConfig obj dflt =
    ReservationConfig {
    marginStart    = marginStart    `or'` (lookupWord   "marginStart"),
    scriptFilePath = scriptFilePath `or'` (lookupString "scriptFilePath")
    }
    where
      mor' = mor dflt obj
      or'  = Config.Class.or dflt obj
