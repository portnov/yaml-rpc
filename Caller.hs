{-# LANGUAGE OverloadedStrings #-}

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

callDynamic :: (IsYamlObject a, IsYamlObject b) => (BS.ByteString -> IO (BS.ByteString,Int)) -> BS.ByteString -> BS.ByteString -> a -> IO b
callDynamic getServer service name args = do
  srv <- getServer service
  call srv name args

call :: (IsYamlObject a, IsYamlObject b) => (BS.ByteString, Int) -> BS.ByteString -> a -> IO b
call (host,port) name args = withSocketsDo $ do
  h <- connectTo (BS.unpack host) (PortNumber $ fromIntegral port)
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
