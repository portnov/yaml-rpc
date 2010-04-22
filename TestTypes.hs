{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses #-}
module TestTypes where

import Data.Default

import Network.YAML.Derive

data Point = Point { x :: Double, y :: Double }
  deriving (Show)

$(deriveDefault ''Point)

$(deriveIsYamlObject ''Point)

