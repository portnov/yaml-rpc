{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}

module TestGen where

import GHC.Generics
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson

import Network.YAML.API
import qualified Network.YAML.TH.Client as C
import Network.YAML.TH.Dispatcher

import qualified TestAPI as Test

$(C.generateAPI Test.testApi)

deriving instance Generic User
instance FromJSON User
instance ToJSON User

