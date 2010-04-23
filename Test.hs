{-# LANGUAGE OverloadedStrings #-}

import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.Map as M
import Control.Concurrent

import Network.YAML.Dispatcher
import Network.YAML.Base
import Network.YAML.Instances
import Network.YAML.Server (forkA)

import TestTypes

double :: Point -> IO Point
double (Point x y) = return $ Point (x*2) (y*2)

mySum :: [Double] -> IO Double
mySum = return . sum

counter :: (Int,Int) -> IO Int
counter (k,d) = do
    mapM count [k..k+10]
    return (k+10)
  where
    count i = do
      putStrLn $ show d ++ ": " ++ show i
      threadDelay (d*100000)

rules = mkRules [("double", yamlMethod double),
                 ("sum",    yamlMethod mySum),
                 ("count",  yamlMethod counter)]

main = do
  putStrLn "Listening..."
  forkA [dispatcher 5000 rules,
         dispatcher 5001 rules,
         dispatcher 5002 rules]
  return ()
