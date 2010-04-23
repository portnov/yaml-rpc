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

-- Declare dispatchingRules for given functions
$(declareRules ['double, 'mySum, 'counter])

main = do
  putStrLn "Listening..."
  -- Start 3 listeners on 3 ports
  forkA [dispatcher 5000 dispatchingRules,
         dispatcher 5001 dispatchingRules,
         dispatcher 5002 dispatchingRules]
  return ()
