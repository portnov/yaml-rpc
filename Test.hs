{-# LANGUAGE OverloadedStrings #-}

import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.Map as M

import Network.YAML.Dispatcher
import Network.YAML.Base
import Network.YAML.Instances

import TestTypes

double :: Point -> IO Point
double (Point x y) = return $ Point (x*2) (y*2)

mySum :: [Double] -> IO Double
mySum = return . sum

rules = mkRules [("double", yamlMethod double),
                 ("sum",    yamlMethod mySum)]

main = do
  putStrLn "Listening..."
  dispatcher 5000 rules
  return ()
