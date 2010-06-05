{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Network.YAML.Base where

import Control.Monad
import Data.Maybe
import Data.Default
import Data.Object
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS
import Text.Libyaml hiding (encode, decode)

import Network.YAML.Types

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

getSubKey :: (IsYamlScalar a) => BS.ByteString -> BS.ByteString -> YamlObject -> Maybe a
getSubKey key subkey obj = do
  attr <- getAttr key obj
  r <- getAttr subkey attr
  getScalar r

getItem :: BS.ByteString -> Int -> YamlObject -> YamlObject
getItem key k obj = 
  case getListAttr key obj of
    [] ->  Sequence []
    lst -> lst !! k

getListAttr :: BS.ByteString -> YamlObject -> [YamlObject]
getListAttr key obj = 
  case getAttr key obj of
    Just x -> getList x
    Nothing -> []

getFirstKey :: YamlObject -> BS.ByteString
getFirstKey (Mapping pairs) = fromYamlScalar $ fst $ head pairs

instance IsYamlScalar Double where
  fromYamlScalar (YamlScalar v _ _) = read $ BS.unpack v
  toYamlScalar x = YamlScalar (BS.pack $ show x) NoTag Any

instance IsYamlScalar Int where
  fromYamlScalar (YamlScalar v _ _) = read $ BS.unpack v
  toYamlScalar x = YamlScalar (BS.pack $ show x) NoTag Any

instance IsYamlScalar Integer where
  fromYamlScalar (YamlScalar v _ _) = read $ BS.unpack v
  toYamlScalar x = YamlScalar (BS.pack $ show x) NoTag Any

serialize :: IsYamlObject a => a -> BS.ByteString
serialize x = 
  let c :: YamlObject
      c = toYaml x
  in  encode c

unserialize :: IsYamlObject a => BS.ByteString -> Maybe a
unserialize x =
  let d :: Maybe YamlObject
      d = decode x
  in  fromYaml `fmap` d

