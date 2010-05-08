{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, OverloadedStrings, FlexibleInstances, IncoherentInstances #-}

module Network.YAML.Instances where
  
import Data.Maybe
import Data.Default
import Data.Object
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

import Network.YAML.Base

-- | Build YamlObject from (key,value) pairs
object :: [(BS.ByteString, YamlScalar)] -> YamlObject
object pairs = Mapping [(toYamlScalar name, Scalar val) | (name,val) <- pairs]

-- | Build YamlObject with single field
field :: (IsYamlScalar a) => BS.ByteString -> a -> YamlObject
field name val = Mapping [(toYamlScalar name, Scalar $ toYamlScalar val)]

instance Default BS.ByteString where
  def = BS.empty

instance (IsYamlObject a) => ConvertSuccess [a] YamlObject where
  convertSuccess lst = Sequence $ map cs lst

instance (IsYamlObject a) => ConvertSuccess YamlObject [a] where
  convertSuccess (Mapping pairs) = map cs $ map snd pairs
  convertSuccess (Sequence lst) = map cs lst
  convertSuccess s@(Scalar _) = [cs s]

instance (IsYamlObject a) => IsYamlObject [a] where

tryGet lst k = 
  if k >= length lst
    then def
    else lst !! k

instance (IsYamlObject a, IsYamlObject b) => ConvertSuccess (a,b) YamlObject where
  convertSuccess (x,y) = Sequence [cs x, cs y]

instance (IsYamlObject a, IsYamlObject b) => ConvertSuccess YamlObject (a,b) where
  convertSuccess obj = (cs x, cs y) 
    where
      list = getList obj
      x = tryGet list 0
      y = tryGet list 1

instance (IsYamlObject a, IsYamlObject b) => IsYamlObject (a,b) where

instance (IsYamlObject a, IsYamlObject b, IsYamlObject c) => ConvertSuccess (a,b,c) YamlObject where
  convertSuccess (x,y,z) = Sequence [cs x, cs y, cs z]

instance (IsYamlObject a, IsYamlObject b, IsYamlObject c) => ConvertSuccess YamlObject (a,b,c) where
  convertSuccess obj = (cs x, cs y, cs z) 
    where
      list = getList obj
      x = tryGet list 0
      y = tryGet list 1
      z = tryGet list 2

instance (IsYamlObject a, IsYamlObject b, IsYamlObject c) => IsYamlObject (a,b,c) where

instance (Default a, Default b) => Default (a,b) where
  def = (def, def)

instance (Default a, Default b, Default c) => Default (a,b,c) where
  def = (def, def, def)

_right :: BS.ByteString
_right = "Right"

_left :: BS.ByteString
_left = "Left"

instance (IsYamlObject a, IsYamlObject b) => ConvertSuccess (Either a b) YamlObject where
  convertSuccess (Right a) = Mapping [(toYamlScalar _right, cs a)]
  convertSuccess (Left b) = Mapping [(toYamlScalar _left, cs b)]

instance (IsYamlObject a, IsYamlObject b) => ConvertSuccess YamlObject (Either a b) where
  convertSuccess (Mapping [(name, val)]) = 
    if fromYamlScalar name == _right 
      then Right (cs val)
      else if fromYamlScalar name == _left
             then Left (cs val)
             else def
  convertSuccess _ = def

instance (Default a) => Default (Either a b) where
  def = Left def

instance (IsYamlObject a, IsYamlObject b) => IsYamlObject (Either a b) where

instance Default YamlObject where
  def = Sequence []

instance IsYamlObject YamlObject where

instance ConvertSuccess YamlObject Double where
  convertSuccess x = fromMaybe def $ getScalar x

instance ConvertSuccess Double YamlObject where
  convertSuccess x = Scalar $ toYamlScalar x

instance IsYamlObject Double where

instance ConvertSuccess YamlObject Int where
  convertSuccess x = fromMaybe def $ getScalar x

instance ConvertSuccess Int YamlObject where
  convertSuccess x = Scalar $ toYamlScalar x

instance IsYamlObject Int where

instance ConvertSuccess YamlObject Integer where
  convertSuccess x = fromMaybe def $ getScalar x

instance ConvertSuccess Integer YamlObject where
  convertSuccess x = Scalar $ toYamlScalar x

instance IsYamlObject Integer where

instance ConvertSuccess YamlObject BS.ByteString where
  convertSuccess x = fromMaybe def $ getScalar x

instance ConvertSuccess BS.ByteString YamlObject where
  convertSuccess x = Scalar $ toYamlScalar x

instance IsYamlObject BS.ByteString where

instance ConvertSuccess YamlObject String where
  convertSuccess x = fromMaybe def $ getScalar x

instance ConvertSuccess String YamlObject where
  convertSuccess x = Scalar $ toYamlScalar x

instance IsYamlObject String where

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

-- | Convert any (a -> IO b) action to YAML RPC method
yamlMethod :: (IsYamlObject a, IsYamlObject b) => (a -> IO b) -> YamlObject -> IO YamlObject
yamlMethod fn = \obj -> do
  let x = cs obj
  y <- fn x
  return $ cs y

