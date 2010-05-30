
module Network.YAML.Balancer where

import System.Random
import qualified Data.ByteString.Char8 as BS

import Network.YAML.Base (HostAndPort)

-- | Select random server
selectRandom :: [(BS.ByteString, HostAndPort, Int)]   -- ^ [(Service name, (hostname, port number), priority)]
             -> BS.ByteString                         -- ^ Service name
             -> IO HostAndPort
selectRandom lst service = do
  let lst' = concatMap (\(name,srv,p) -> replicate p (name, srv)) lst
      lst'' = map snd $ filter (\(name,srv) -> name==service) lst'
      n = length lst''
  k <- randomRIO (0, n-1)
  return $ lst'' !! k

