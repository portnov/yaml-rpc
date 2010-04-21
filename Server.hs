
module Server where

import Control.Monad 
import Control.Monad.State
import Control.Concurrent
import Control.Exception
import Network
import System.IO
import qualified Data.ByteString.Char8 as BS
import Data.Object.Yaml

import YAML
import YAMLInstances

(<+>) = BS.append

readHandle :: Handle -> [BS.ByteString] -> IO [BS.ByteString]
readHandle h acc = do
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

server ::
      Int
   -> (YamlObject -> IO YamlObject)
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
                    print ob
                    res <- callOut ob
                    BS.hPutStrLn h $ serialize res
                    hClose h)

