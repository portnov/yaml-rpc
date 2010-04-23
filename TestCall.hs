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

ps = [Point 3.0 5.0, Point 1.0 2.1, Point 0.1 0.2]

main = do
  srv <- getService "test"
  r <- call srv "double" p
  print (r :: Point)
  s <- call srv "sum" ([3.5, 5.5, 1.0] :: [Double])
  print (s :: Double)
  rs <- callP getService "test" "double" ps
  print (rs :: [Point])
