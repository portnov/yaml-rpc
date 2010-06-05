
module Network.YAML.Dispatcher where

import qualified Data.Map as M
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

import Network.YAML.Types
import Network.YAML.Instances
import Network.YAML.Server

-- | Build dispatching rules
mkRules :: [(BS.ByteString, Worker)] -> Rules
mkRules pairs = M.fromList pairs

-- | Select worker from dispatching rules
dispatch :: Rules -> Worker
dispatch rules = \obj -> 
  let call :: Call
      call = fromYaml obj
  in case M.lookup (methodName call) rules of
      Nothing -> fail $ "Unknown method: " ++ (BS.unpack $ methodName call)
      Just fn -> fn (args call)

-- | Listens given port and dispatches requests
dispatcher :: Int -> Rules -> IO ()
dispatcher port rules = server port (dispatch rules)

-- | Similar, but use persistent server.
persistentDispatcher :: Int -> Rules -> IO ()
persistentDispatcher port rules = persistentServer port (dispatch rules)
