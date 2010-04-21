{-# LANGUAGE OverloadedStrings #-}

import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.Map as M

import Dispatcher
import YAML
import YAMLInstances

double :: Point -> IO Point
double (Point x y) = return $ Point (x*2) (y*2)

rules = mkRules [("double", yamlMethod double)]

main = do
  putStrLn "Listening..."
  dispatcher 5000 rules
  return ()
