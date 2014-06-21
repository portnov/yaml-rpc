{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}

module Test.Server where

import Web.Scotty

import Network.YAML
import Network.YAML.Scotty

import Test.TestAPIImpl

$(generateDispatcher api)

main :: IO ()
main = scotty 3000 $ do
  servePost dispatcher

