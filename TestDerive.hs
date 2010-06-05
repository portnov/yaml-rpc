{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Default
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

import Network.YAML.Base
import Network.YAML.Derive

data Test = Test {getX :: Int, getY :: Int}
          | Another {getA :: Double}
          | Third Int Double
          | Fourth Double Test
  deriving(Show)

$(deriveDefault ''Test)

$(deriveIsYamlObject ''Test)

t1 = Test 3 5
t2 = Another 7.5
t3 = Third 2 1.5
t4 = Fourth 1.7 t1

test :: Test -> IO ()
test t = do
  let s = serialize t
  BS.putStrLn s
  print (unserialize s :: Maybe Test)

main = do
  test t1
  test t2
  test t3
  test t4

