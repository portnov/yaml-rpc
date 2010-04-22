{-# LANGUAGE OverloadedStrings #-}

import Data.Object.Yaml
import Data.Convertible.Base

import Network.YAML.Base
import Network.YAML.Instances
import Network.YAML.Caller

import TestTypes

getService "test" = return ("127.0.0.1", 5000)
getService _ = fail "Unknown service"

p = Point 2.0 3.0

main = do
  srv <- getService "test"
  r <- call srv "double" p
  print (r :: Point)
