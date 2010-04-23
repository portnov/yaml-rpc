{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.Map as M

import Network.YAML.Dispatcher
import Network.YAML.Base
import Network.YAML.Instances
import Network.YAML.Server (forkA)
import Network.YAML.WrapMethods

import TestTypes
import Methods

$(declareRules ['double, 'mySum, 'counter])

main = do
  putStrLn "Listening..."
  forkA [dispatcher 5000 dispatchingRules,
         dispatcher 5001 dispatchingRules,
         dispatcher 5002 dispatchingRules]
  return ()
