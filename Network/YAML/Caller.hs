{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}

module Network.YAML.Caller where

import qualified Data.Map as M
import Data.Object.Yaml
import Data.Convertible.Base
import qualified Data.ByteString.Char8 as BS
import Network
import System.IO
import Control.Monad
import Control.Concurrent

import Network.YAML.Base
import Network.YAML.Instances
import Network.YAML.Server

class Connection c where
  newConnection :: (BS.ByteString, Int) -> IO c
  closeConnection :: c -> IO ()
  -- | Call remote method
  call :: (IsYamlObject a, IsYamlObject b)
       => c
       -> BS.ByteString                   -- ^ Name of method
       -> a                               -- ^ Argument for method
       -> IO b

-- | Send any YAML text and return an answer
sendYAML :: (BS.ByteString, Int)      -- ^ (Hostname, port)
         -> BS.ByteString             -- ^ YAML text
         -> IO BS.ByteString          -- ^ Answer
sendYAML (host,port) yaml =  withSocketsDo $ do
  h <- connectTo (BS.unpack host) (PortNumber $ fromIntegral port)
  hSetBuffering h NoBuffering
  BS.hPutStrLn h yaml
  lns <- readHandle h []
  hClose h
  let text = BS.unlines lns
  return text

-- | Send any YAML text and return an answer
hSendYAML :: Handle
         -> BS.ByteString             -- ^ YAML text
         -> IO BS.ByteString          -- ^ Answer
hSendYAML h yaml =  withSocketsDo $ do
  hSetBuffering h NoBuffering
  BS.hPutStrLn h yaml
  lns <- readHandle h []
  let text = BS.unlines lns
  return text

instance Connection HostAndPort where
  -- | Call remote method
--   call :: (IsYamlObject a, IsYamlObject b)
--        => (BS.ByteString, Int)            -- ^ (Host name, port number)
--        -> BS.ByteString                   -- ^ Name of method
--        -> a                               -- ^ Argument for method
--        -> IO b
  call (host,port) name args = do
    let c = mkCall name (cs args)
        s = serialize c
    text <- sendYAML (host,port) s
    case unserialize text of
      Nothing -> fail "No answer"
      Just x -> return x

  newConnection pair = return pair
  closeConnection _ = return ()

newtype PersistentConnection = PC Handle

instance Connection PersistentConnection where
  newConnection (host, port) = do
    h <- connectTo (BS.unpack host) (PortNumber $ fromIntegral port)
    return (PC h)

  closeConnection (PC h) = hClose h

  call (PC h) name args = do
    let c = mkCall name (cs args)
        s = serialize c
    text <- hSendYAML h s
    case unserialize text of
      Nothing -> fail "No answer"
      Just x -> return x

-- | Similar to call, but select server on each call
callDynamic :: (IsYamlObject a, IsYamlObject b)
            => (BS.ByteString -> IO (BS.ByteString,Int)) -- ^ Get (Host name, port number) from service name
            -> BS.ByteString                             -- ^ Name of the service
            -> BS.ByteString                             -- ^ Name of method
            -> a                                         -- ^ Argument for method
            -> IO b
callDynamic getServer service name args = do
  srv <- getServer service
  call srv name args

-- | Call a method and put it's result into MVar
callF :: (IsYamlObject a, IsYamlObject b)
      => (BS.ByteString -> IO (BS.ByteString, Int))           -- ^ Get (Host, port) from service name
      -> BS.ByteString                                        -- ^ Service name
      -> BS.ByteString                                        -- ^ Method name
      -> (a, MVar b)                                          -- ^ (Argument, MVar for result)
      -> IO ()
callF getServer service name (args, var) = do
  srv <- getServer service
--   putStrLn $ "Calling to " ++ show srv
  r <- call srv name args
  putMVar var r

-- | Call a method for each argument in the list in parallel
-- (it can run method for each argument on another server)
callP :: (IsYamlObject a, IsYamlObject b)
      => (BS.ByteString -> IO (BS.ByteString, Int))      -- ^ Get (Host, port) from service name
      -> BS.ByteString                                   -- ^ Service name
      -> BS.ByteString                                   -- ^ Method name
      -> [a]                                             -- ^ List of arguments
      -> IO [b]
callP getServer service name args = do
  let n = length args
  vars <- replicateM n newEmptyMVar
  mapM (forkIO . callF getServer service name) $ zip args vars
  mapM takeMVar vars

