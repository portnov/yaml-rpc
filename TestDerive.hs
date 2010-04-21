{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}

import Data.Convertible.Base
import Data.Object.Yaml

import YAML
import Derive

data Test = Test {getX :: Int, getY :: Int}

$(deriveToYamlObject ''Test)

t1 = Test 3 5

t2 :: YamlObject
t2 = cs t1

main = print t2

