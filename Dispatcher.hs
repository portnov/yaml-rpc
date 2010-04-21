
module Dispatcher where

import qualified Data.Map as M
import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.ByteString.Char8 as BS

import YAML
import YAMLInstances
import Server

type Worker = YamlObject -> IO YamlObject
type Rules = M.Map BS.ByteString Worker

mkRules :: [(String,Worker)] -> Rules
mkRules pairs = M.fromList [(BS.pack name, worker) | (name,worker) <- pairs]

dispatch :: Rules -> Worker
dispatch rules = \obj -> 
  let call :: Call
      call = cs obj
  in case M.lookup (methodName call) rules of
      Nothing -> fail $ "Unknown method: " ++ (BS.unpack $ methodName call)
      Just fn -> fn (args call)

dispatcher :: Int -> Rules -> IO ()
dispatcher port rules = server port (dispatch rules)
