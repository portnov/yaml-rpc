{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}

module Test.Server where

import Web.Scotty

import Network.YAML.API
import qualified Network.YAML.TH.Server as S
import Network.YAML.TH.Dispatcher
import Network.YAML.Scotty

import Test.TestAPIImpl

$(generateDispatcher api)

main :: IO ()
main = scotty 3000 $ do
  servePost dispatcher
