{-# LANGUAGE OverloadedStrings #-}

module Network.YAML.Scotty where

import Control.Monad.IO.Class
import Web.Scotty
import Network.HTTP.Types
import qualified Data.Text as T
import Data.Aeson hiding (json)

import Network.YAML.API
import Network.YAML.TH.Dispatcher

servePost :: Dispatcher -> ScottyM ()
servePost dispatcher = post "/:method" $ do
    methodName <- param "method"
    case dispatcher methodName of
      Nothing -> status status404
      Just method -> do
          args <- jsonData
          result <- liftIO $ method args
          json result

