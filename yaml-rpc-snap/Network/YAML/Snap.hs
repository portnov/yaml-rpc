{-# LANGUAGE OverloadedStrings #-}

module Network.YAML.Snap where

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Network.YAML
import qualified Data.Aeson as Json
import Snap

errorMsg :: Int -> B.ByteString -> Snap ()
errorMsg status msg = do
  modifyResponse $ setResponseStatus status msg
  writeBS msg
  finishWith =<< getResponse

servePost :: Dispatcher IO -> Snap ()
servePost dispatcher = do
  maybeMethod <- getParam "method"
  case maybeMethod of
    Nothing -> errorMsg 400 "No method name specified"
    Just methodName -> case dispatcher (TE.decodeUtf8 methodName) of
                         Nothing -> errorMsg 404 "No such method"
                         Just method -> do
                          body <- readRequestBody 16384
                          case Json.decode body of
                            Nothing -> errorMsg 400 "Invalid JSON in request"
                            Just json -> do
                              result <- liftIO $ method json
                              writeLBS $ Json.encode result
