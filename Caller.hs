
module Caller where

import qualified Data.Map as M
import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.ByteString.Char8 as BS
import Network
import System.IO

import YAML
import YAMLInstances
import Server

call :: (IsYamlObject a, IsYamlObject b) => (String -> IO (String,Int)) -> String -> String -> a -> IO b
call getServer service name args = withSocketsDo $ do
  (host,port) <- getServer service
  h <- connectTo host (PortNumber $ fromIntegral port)
  let c = mkCall name (cs args)
      s = serialize c
  hSetBuffering h NoBuffering
  BS.hPutStrLn h s
  lns <- readHandle h []
  hClose h
  let text = BS.unlines lns
  case unserialize text of
    Nothing -> fail "No answer"
    Just x -> return x
