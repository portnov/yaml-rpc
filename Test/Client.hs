{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}

module Test.Client where

import GHC.Generics
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson

import Network.YAML

import qualified Test.TestAPI as Test

$(useAPI "test.api")

deriving instance Generic User
deriving instance Show User
instance FromJSON User
instance ToJSON User

deriving instance Generic Something
instance FromJSON Something
instance ToJSON Something

main :: IO ()
main = do
  let url = "http://localhost:3000" :: String
  result <- testSmth url "zzzz" (Something {smthText = "ivan", smthList = ["Ivan", "Ivanov"]})
  print result

