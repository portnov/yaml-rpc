{-# LANGUAGE OverloadedStrings #-}

module Network.YAML.Server where

import Control.Monad 
import Control.Monad.State
import Control.Concurrent
import Control.Exception
import Network
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Object.Yaml

import Network.YAML.Base
import Network.YAML.Instances

-- | Run each IO action in separate thread and return all results
forkA :: [IO a] -> IO [a]
forkA lst = do
    let n = length lst
    vars <- replicateM n newEmptyMVar
    mapM (forkIO . run) $ zip lst vars
    mapM takeMVar vars
  where
    run (x,v) = do
      r <- x
      putMVar v r

-- | Read lines from Handle
readHandle :: Handle
           -> [BS.ByteString]       -- ^ Already read lines
           -> IO [BS.ByteString]
readHandle h acc = do
  eof <- hIsEOF h
  if eof
    then return acc
    else do
      line <- BS.hGetLine h
      let line' = if BS.null line
                    then line
                    else if (BS.last line)=='\r'
                            then BS.init line
                            else line
  --           print $ "read line:"++line'
      if BS.null line'
        then return acc
        else readHandle h (acc ++ [line'])

-- | Start server and wait for connections
server ::
      Int                              -- ^ Port number
   -> (YamlObject -> IO YamlObject)    -- ^ Worker
   -> IO ()
server port callOut = do
--        installHandler sigPIPE Ignore Nothing    
      sock  <- listenOn (PortNumber $ fromIntegral port)
      (forever $ loop sock) `finally` sClose sock
  where
    loop :: Socket -> IO ThreadId
    loop sock =
         do (h,_nm,_port) <- accept sock
            forkIO
              (do 
                hSetBuffering h NoBuffering
                lns <- readHandle h []
                let text = BS.unlines lns
                case unserialize text of
                  Nothing -> hClose h
                  Just ob -> do
--                     print ob
                    res <- callOut ob
                    BS.hPutStrLn h $ serialize res
                    hClose h)

persistentServer :: 
      Int 
   -> (YamlObject -> IO YamlObject)
   -> IO ()
persistentServer port callOut = do
--        installHandler sigPIPE Ignore Nothing    
      sock  <- listenOn (PortNumber $ fromIntegral port)
      (forever $ loop sock) `finally` sClose sock
  where
    loop :: Socket -> IO ThreadId
    loop sock =
         do (h,_nm,_port) <- accept sock
            forkIO (worker h)

    worker :: Handle -> IO ()
    worker h = do 
      hSetBuffering h NoBuffering
      lns <- readHandle h []
      let text = BS.unlines lns
      if BS.null text
        then hClose h
        else
          case unserialize text of
            Nothing -> hClose h
            Just ob -> do
              res <- callOut ob
              BS.hPutStrLn h $ serialize res
              if getScalarAttr "connection" ob == Just ("close" :: BS.ByteString)
                then hClose h
                else worker h

