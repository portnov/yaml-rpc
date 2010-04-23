
module Network.YAML.Balancer where

import System.Random
import qualified Data.ByteString.Char8 as BS

type Server = (BS.ByteString, Int)

selectRandom :: [(BS.ByteString, Server, Int)] -> BS.ByteString -> IO Server
selectRandom lst service = do
  let lst' = concatMap (\(name,srv,p) -> replicate p (name, srv)) lst
      lst'' = map snd $ filter (\(name,srv) -> name==service) lst'
      n = length lst''
  k <- randomRIO (0, n-1)
  return $ lst'' !! k

