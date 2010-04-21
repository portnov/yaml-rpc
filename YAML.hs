{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances, MultiParamTypeClasses, OverloadedStrings #-}

module YAML where

import Control.Monad
import Data.Maybe
-- import Data.Convertible
import Data.Object
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS
import Text.Libyaml hiding (encode, decode)

class (ConvertSuccess YamlObject a, ConvertSuccess a YamlObject) => IsYamlObject a where

getAttr :: BS.ByteString -> YamlObject -> Maybe YamlObject
getAttr key (Mapping pairs) = lookup (toYamlScalar key) pairs
getAttr key (Sequence lst) =
  case catMaybes $ map (getAttr key) lst of
    [x] -> Just x
    _   -> Nothing
getAttr key (Scalar sc) = Nothing

getScalar :: (IsYamlScalar a) => YamlObject -> Maybe a 
getScalar (Scalar x) = Just (fromYamlScalar x)
getScalar _          = Nothing

getList :: YamlObject -> [YamlObject]
getList (Sequence lst) = lst
getList _              = []

getScalarAttr :: (IsYamlScalar a) => BS.ByteString -> YamlObject -> Maybe a
getScalarAttr key obj = getScalar =<< getAttr key obj

getListAttr :: BS.ByteString -> YamlObject -> [YamlObject]
getListAttr key obj = 
  case getAttr key obj of
    Just x -> getList x
    Nothing -> []

instance IsYamlScalar Double where
  fromYamlScalar (YamlScalar v _ _) = read $ BS.unpack v
  toYamlScalar x = YamlScalar (BS.pack $ show x) NoTag Any

instance IsYamlScalar Int where
  fromYamlScalar (YamlScalar v _ _) = read $ BS.unpack v
  toYamlScalar x = YamlScalar (BS.pack $ show x) NoTag Any

serialize :: IsYamlObject a => a -> BS.ByteString
serialize x = 
  let c :: YamlObject
      c = cs x
  in  encode c

unserialize :: IsYamlObject a => BS.ByteString -> Maybe a
unserialize x =
  let d :: Maybe YamlObject
      d = decode x
  in  case d of
        Just y -> Just $ cs y
        Nothing -> Nothing

-- p = Point 3.0 2.0
-- 
-- main = do
--   let s = serialize p
--   BS.putStrLn s
--   let c :: YamlObject
--       c = cs p
--       p' :: Point
--       p' = cs c
--   print p'
