{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}

module Test.Server where

import Data.Monoid
import qualified Data.ByteString as B
import Snap

import Network.YAML
import Network.YAML.Snap

import Test.TestAPIImpl

$(generateDispatcher api)

main :: IO ()
main = httpServe (setPort 3000 mempty) site

site :: Snap ()
site = route [("/:method", servePost dispatcher)]
