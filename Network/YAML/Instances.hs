{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Network.YAML.Instances where
  
import Data.Maybe
import Data.Default
import Data.Object
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

import Network.YAML.Base

object :: [(BS.ByteString, YamlScalar)] -> YamlObject
object pairs = Mapping [(toYamlScalar name, Scalar val) | (name,val) <- pairs]

field :: (IsYamlScalar a) => BS.ByteString -> a -> YamlObject
field name val = Mapping [(toYamlScalar name, Scalar $ toYamlScalar val)]

instance (IsYamlObject a) => ConvertSuccess [a] YamlObject where
  convertSuccess lst = Sequence $ map cs lst

instance (IsYamlObject a) => ConvertSuccess YamlObject [a] where
  convertSuccess (Mapping pairs) = map cs $ map snd pairs
  convertSuccess (Sequence lst) = map cs lst
  convertSuccess s@(Scalar _) = [cs s]

instance (IsYamlObject a) => IsYamlObject [a] where

instance Default YamlObject where
  def = Sequence []

instance IsYamlObject YamlObject where

data Call = Call { methodName :: BS.ByteString, args :: YamlObject }
  deriving (Show)

mkCall :: BS.ByteString -> YamlObject -> YamlObject
mkCall name args = cs $ Call name args

stringScalar :: String -> YamlScalar
stringScalar = toYamlScalar

instance ConvertSuccess Call YamlObject where
  convertSuccess (Call name args) = Mapping [(stringScalar "call", Scalar $ toYamlScalar name), 
                                             (stringScalar "args", args)]

instance ConvertSuccess YamlObject Call where
  convertSuccess obj = Call name args
    where
      name = fromMaybe "defaultMethod" $ getScalarAttr "call" obj
      args = fromMaybe (Sequence []) $ getAttr "args" obj

instance Default Call where
  def = Call "defaultMethod" def

instance IsYamlObject Call where

yamlMethod :: (IsYamlObject a, IsYamlObject b) => (a -> IO b) -> YamlObject -> IO YamlObject
yamlMethod fn = \obj -> do
  let x = cs obj
  y <- fn x
  return $ cs y

