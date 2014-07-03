{-# LANGUAGE OverloadedStrings #-}

module Network.YAML.Scotty (servePost, servePostT, handleApi, handleApiT) where

import Control.Monad.Morph
import Control.Monad.IO.Class
import Web.Scotty
import qualified Web.Scotty.Trans as Trans
import Network.HTTP.Types
import qualified Data.Text as T
import Data.Aeson hiding (json)

import Network.YAML.API
import Network.YAML.TH.Dispatcher

-- | Scotty handler. Serves each method on @/:method@.
servePost :: Dispatcher IO -> ScottyM ()
servePost dispatcher = post "/:method" $ handleApi dispatcher

handleApi :: Dispatcher IO -> ActionM ()
handleApi dispatcher = do
    methodName <- param "method"
    case dispatcher methodName of
      Nothing -> status status404
      Just method -> do
          args <- jsonData
          result <- liftIO $ method args
          json result

-- | Scotty handler. Serves each method on @/:method@.
servePostT :: (Trans.ScottyError e, MonadIO m) => Dispatcher m -> Trans.ScottyT e m ()
servePostT dispatcher = Trans.post "/:method" $ handleApiT dispatcher

handleApiT :: (Trans.ScottyError e, MonadIO m) => Dispatcher m -> Trans.ActionT e m ()
handleApiT dispatcher = do
    methodName <- Trans.param "method"
    case dispatcher methodName of
      Nothing -> Trans.status status404
      Just method -> do
          args <- Trans.jsonData
          result <- lift $ method args
          Trans.json result

