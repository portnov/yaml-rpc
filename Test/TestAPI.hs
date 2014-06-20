{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Test.TestAPI where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

import Network.YAML.API
import qualified Network.YAML.TH.Server as S

data User = User {userLogin :: T.Text, userFullName :: T.Text}

testApi :: API
testApi = $(readAPI "test.api")

