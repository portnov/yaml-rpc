{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Default
import Data.Convertible.Base
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

import YAML
import Derive

data Test = Test {getX :: Int, getY :: Int}
          | Another {getA :: Double}
  deriving(Show)

$(deriveDefault ''Test)

$(deriveIsYamlObject ''Test)

t1 = Test 3 5
t2 = Another 7.5

test :: Test -> IO ()
test t = do
  let s = serialize t
  BS.putStrLn s
  print (unserialize s :: Maybe Test)

main = do
  test t1
  test t2

