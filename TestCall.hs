{-# LANGUAGE OverloadedStrings, TemplateHaskell, ScopedTypeVariables #-}
-- | Test client
module Main where

import Control.Monad
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS
import Data.Object.Yaml

import Network.YAML

import TestTypes
import qualified Methods

-- declare `double', `mySum' and `ls' as RPC methods
$(remote 'Methods.double)
$(remote 'Methods.mySum)
$(remote 'Methods.ls)
-- For example, `ls' is defined in Methods.hs as
-- ls :: String -> IO [String]
-- Now `ls' is defined here as
-- ls :: (Connection c) => c -> String -> IO [String]


rules host = [("test", (host', 5000), 1),
              ("test", (host', 5001), 1),
              ("test", (host', 5002), 1)]
  where
    host' = BS.pack host

getService host = selectRandom (rules host)

p = Point 2.0 3.0

ps = [Point 3.0 5.0, Point 1.0 2.1, Point 0.1 0.2]

main = do
  [host] <- getArgs
  test <- getService host "test"

  (conn :: PersistentConnection) <- newConnection test
--   (conn :: HostAndPort) <- newConnection test

  -- call remote functions
  replicateM 100 $ do
    r <- double conn p
    print r

--   s <- mySum conn [3.5, 5.5, 1.0]
--   print s
--   lst <- ls conn "/tmp"
--   print lst
-- 
--   -- call remote functions for many arguments, for each argument on different server maybe
--   rs <- callP (getService host) "test" "double" ps
--   print (rs :: [Point])
--   cs <- callP (getService host) "test" "counter" $ zip ([3,4,5,6] :: [Int]) ([1..] :: [Int])
--   print (cs :: [Int])

  closeConnection conn
