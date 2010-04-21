{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module YAMLInstances where
  
import Data.Maybe
import Data.Object
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

import YAML

data Point = Point { x :: Double, y :: Double }
  deriving (Show)

object :: [(String, YamlScalar)] -> YamlObject
object pairs = Mapping [(toYamlScalar name, Scalar val) | (name,val) <- pairs]

field :: (IsYamlScalar a) => String -> a -> YamlObject
field name val = Mapping [(toYamlScalar name, Scalar $ toYamlScalar val)]

instance (IsYamlObject a) => ConvertSuccess [a] YamlObject where
  convertSuccess lst = Sequence $ map cs lst

instance (IsYamlObject a) => ConvertSuccess YamlObject [a] where
  convertSuccess (Mapping pairs) = map cs $ map snd pairs
  convertSuccess (Sequence lst) = map cs lst
  convertSuccess s@(Scalar _) = [cs s]

instance (IsYamlObject a) => IsYamlObject [a] where

instance ConvertSuccess Point YamlObject where
  convertSuccess (Point x y) = object [("x", toYamlScalar x),
                                       ("y", toYamlScalar y)]

instance ConvertSuccess YamlObject Point where
  convertSuccess obj = Point x y
    where
      x = fromMaybe 0 $ getScalarAttr "x" obj
      y = fromMaybe 0 $ getScalarAttr "y" obj

instance IsYamlObject Point where

instance IsYamlObject YamlObject where

data Call = Call { methodName :: BS.ByteString, args :: YamlObject }
  deriving (Show)

mkCall :: String -> YamlObject -> YamlObject
mkCall name args = cs $ Call (BS.pack name) args

instance ConvertSuccess Call YamlObject where
  convertSuccess (Call name args) = Mapping [(toYamlScalar "call", Scalar $ toYamlScalar name), 
                                             (toYamlScalar "args", args)]

instance ConvertSuccess YamlObject Call where
  convertSuccess obj = Call name args
    where
      name = fromMaybe (BS.pack "defaultMethod") $ getScalarAttr "call" obj
      args = fromMaybe (Sequence []) $ getAttr "args" obj

instance IsYamlObject Call where

yamlMethod :: (IsYamlObject a, IsYamlObject b) => (a -> IO b) -> YamlObject -> IO YamlObject
yamlMethod fn = \obj -> do
  let x = cs obj
  y <- fn x
  return $ cs y

