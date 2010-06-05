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

instance (IsYamlObject a) => IsYamlObject [a] where
  toYaml lst = Sequence $ map toYaml lst

  fromYaml (Mapping pairs) = map fromYaml $ map snd pairs
  fromYaml (Sequence lst) = map fromYaml lst
  fromYaml s@(Scalar _) = [fromYaml s]

tryGet lst k = 
  if k >= length lst
    then def
    else lst !! k

instance (IsYamlObject a, IsYamlObject b) => IsYamlObject (a,b) where
  toYaml (x,y) = Sequence [toYaml x, toYaml y]

  fromYaml obj = (fromYaml x, fromYaml y) 
    where
      list = getList obj
      x = tryGet list 0
      y = tryGet list 1

instance (IsYamlObject a, IsYamlObject b, IsYamlObject c) => IsYamlObject (a,b,c) where
  toYaml (x,y,z) = Sequence [toYaml x, toYaml y, toYaml z]

  fromYaml obj = (fromYaml x, fromYaml y, fromYaml z) 
    where
      list = getList obj
      x = tryGet list 0
      y = tryGet list 1
      z = tryGet list 2

instance (Default a, Default b) => Default (a,b) where
  def = (def, def)

instance (Default a, Default b, Default c) => Default (a,b,c) where
  def = (def, def, def)

_right :: BS.ByteString
_right = "Right"

_left :: BS.ByteString
_left = "Left"

instance (Default a) => Default (Either a b) where
  def = Left def

instance (IsYamlObject a, IsYamlObject b) => IsYamlObject (Either a b) where
  toYaml (Right a) = Mapping [(toYamlScalar _right, toYaml a)]
  toYaml (Left b) = Mapping [(toYamlScalar _left, toYaml b)]

  fromYaml (Mapping [(name, val)]) = 
    if fromYamlScalar name == _right 
      then Right (fromYaml val)
      else if fromYamlScalar name == _left
             then Left (fromYaml val)
             else def
  fromYaml _ = def

instance Default YamlObject where
  def = Sequence []

instance IsYamlObject YamlObject where
  toYaml = id
  fromYaml = id

instance IsYamlObject Double where
  fromYaml x = fromMaybe def $ getScalar x
  toYaml x = Scalar $ toYamlScalar x

instance IsYamlObject Int where
  fromYaml x = fromMaybe def $ getScalar x
  toYaml x = Scalar $ toYamlScalar x

instance IsYamlObject Integer where
  fromYaml x = fromMaybe def $ getScalar x
  toYaml x = Scalar $ toYamlScalar x

instance IsYamlScalar Bool where
  toYamlScalar True = stringScalar "True"
  toYamlScalar False = stringScalar "False"

  fromYamlScalar x = 
    case fromYamlScalar x :: String of
      "True" -> True
      _      -> False

instance Default Bool where
  def = False

instance IsYamlObject Bool where
  toYaml x = Scalar $ toYamlScalar x
  fromYaml x = fromMaybe def $ getScalar x

instance IsYamlObject BS.ByteString where
  fromYaml x = fromMaybe def $ getScalar x
  toYaml x = Scalar $ toYamlScalar x

instance IsYamlObject String where
  fromYaml x = fromMaybe def $ getScalar x
  toYaml x = Scalar $ toYamlScalar x

data Call = Call { methodName :: BS.ByteString, args :: YamlObject }
  deriving (Show)

mkCall :: BS.ByteString -> YamlObject -> YamlObject
mkCall name args = toYaml $ Call name args

stringScalar :: String -> YamlScalar
stringScalar = toYamlScalar

instance Default Call where
  def = Call "defaultMethod" def

instance IsYamlObject Call where
  toYaml (Call name args) = Mapping [(stringScalar "call", Scalar $ toYamlScalar name), 
                                             (stringScalar "args", args)]
  fromYaml obj = Call name args
    where
      name = fromMaybe "defaultMethod" $ getScalarAttr "call" obj
      args = fromMaybe (Sequence []) $ getAttr "args" obj

-- | Convert any (a -> IO b) action to YAML RPC method
yamlMethod :: (IsYamlObject a, IsYamlObject b) => (a -> IO b) -> YamlObject -> IO YamlObject
yamlMethod fn = \obj -> do
  let x = fromYaml obj
  y <- fn x
  return $ toYaml y

