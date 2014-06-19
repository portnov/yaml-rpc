{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses #-}
-- | Declare types for using in test server and test client.
module TestTypes where

import Data.Default

import Network.YAML.Derive

data Point = Point { x :: Double, y :: Double }
  deriving (Show)

-- instance Default Point ...
deriveDefault ''Point

-- instance IsYamlObject Point ...
deriveIsYamlObject ''Point

