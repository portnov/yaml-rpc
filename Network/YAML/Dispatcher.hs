
module Network.YAML.Dispatcher where

import qualified Data.Map as M
import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.ByteString.Char8 as BS

import Network.YAML.Base
import Network.YAML.Instances
import Network.YAML.Server

type Worker = YamlObject -> IO YamlObject
type Rules = M.Map BS.ByteString Worker

-- | Build dispatching rules
mkRules :: [(BS.ByteString,Worker)] -> Rules
mkRules pairs = M.fromList pairs

-- | Select worker from dispatching rules
dispatch :: Rules -> Worker
dispatch rules = \obj -> 
  let call :: Call
      call = cs obj
  in case M.lookup (methodName call) rules of
      Nothing -> fail $ "Unknown method: " ++ (BS.unpack $ methodName call)
      Just fn -> fn (args call)

-- | Listens given port and dispatches requests
dispatcher :: Int -> Rules -> IO ()
dispatcher port rules = server port (dispatch rules)

persistentDispatcher :: Int -> Rules -> IO ()
persistentDispatcher port rules = persistentServer port (dispatch rules)
