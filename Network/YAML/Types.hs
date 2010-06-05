module Network.YAML.Types where

import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Object.Yaml
import Data.Default
import qualified Data.Map as M

-- | This class guaranties that type can be converted to YamlObject and vice versa.
class (Default a) => IsYamlObject a where
  toYaml :: a -> YamlObject
  fromYaml :: YamlObject -> a

-- | Class for different types of connection to RPC servers
class Connection c where
  newConnection :: HostAndPort -> IO c
  closeConnection :: c -> IO ()
  -- | Call remote method
  call :: (IsYamlObject a, IsYamlObject b)
       => c
       -> BS.ByteString                   -- ^ Name of method
       -> a                               -- ^ Argument for method
       -> IO b

-- | (Host name, port number)
type HostAndPort = (BS.ByteString, Int)
type Worker = YamlObject -> IO YamlObject
-- | Service name -> Worker
type Rules = M.Map BS.ByteString Worker

-- | RPC call
data Call = Call { methodName :: BS.ByteString, args :: YamlObject }
  deriving (Show)

newtype PersistentConnection = PC Handle

