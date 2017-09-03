{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Controllers.HomeController(
  multi
  ) where
import Controller.Types(Action)
import Controller(defaultControllerResponse, responseBadRequest)
--import Routing(findRoute)
import qualified DB

multi :: Action ()
multi _ method conn req = return $ responseBadRequest ("urls" :: String)

