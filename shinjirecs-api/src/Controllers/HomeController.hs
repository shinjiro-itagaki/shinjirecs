{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Controllers.HomeController(
  ) where
import Controller.Types(Action)
import Controller(defaultControllerResponse, responseBadRequest)
--import Routing(findRoute)
import qualified DB
