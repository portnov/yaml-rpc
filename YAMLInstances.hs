{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module YAMLInstances where
  
import Data.Maybe
-- import Data.Convertible
import Data.Object
import Data.Object.Yaml

import YAML

data Point = Point { x :: Double, y :: Double }
  deriving (Show)

instance ConvertSuccess Point YamlObject where
  convertSuccess (Point x y) = Mapping [(toYamlScalar "x", Scalar $ toYamlScalar x),
                                        (toYamlScalar "y", Scalar $ toYamlScalar y)]

instance ConvertSuccess YamlObject Point where
  convertSuccess obj = Point x y
    where
      x = fromMaybe 0 $ getScalarAttr "x" obj
      y = fromMaybe 0 $ getScalarAttr "y" obj

instance IsYamlObject Point where

instance IsYamlObject YamlObject where

