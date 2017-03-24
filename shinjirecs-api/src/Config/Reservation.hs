{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.Reservation where
import Config.Class(ConfigClass)

data ReservationCommandArg = ArgDevice | ArgChannel | ArgDurationSec | ArgDestFilePath deriving (Show)

data ReservationConfig = ReservationConfig {
  marginStart :: Word,
  script :: String,
  args :: [ReservationCommandArg]
  } deriving (Show)

defaultReservationConfig :: ReservationConfig
defaultReservationConfig = ReservationConfig {
    marginStart = 3,
    script = "recpt1.sh",
    args = [ArgDevice,ArgChannel,ArgDurationSec,ArgDestFilePath]
    }
