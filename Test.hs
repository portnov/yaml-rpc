{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Test server
module Main where

import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.Map as M

import Network.YAML

import TestTypes
import Methods

st :: State
st = "test"

-- Declare dispatchingRules for given functions
$(declareRulesWithArg 'st ['double, 'mySum, 'counter, 'ls])

main = do
  putStrLn "Listening..."
  -- Start 3 listeners on 3 ports
  forkA [dispatcher 5000 dispatchingRules,
         dispatcher 5001 dispatchingRules,
         dispatcher 5002 dispatchingRules]
  return ()
