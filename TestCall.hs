{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Test client
module Main where

import Data.Object.Yaml
import Data.Convertible.Base

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
-- ls :: (ByteString,Int) -> String -> IO [String]

rules = [("test", ("127.0.0.1", 5000), 1),
         ("test", ("127.0.0.1", 5001), 1),
         ("test", ("127.0.0.1", 5002), 1)]

getService = selectRandom rules

p = Point 2.0 3.0

ps = [Point 3.0 5.0, Point 1.0 2.1, Point 0.1 0.2]

main = do
  test <- getService "test"

  -- call remote functions
  r <- double test p
  print r
  s <- mySum test [3.5, 5.5, 1.0]
  print s
  lst <- ls test "/tmp"
  print lst

  -- call remote functions for many arguments, for each argument on different server maybe
  rs <- callP getService "test" "double" ps
  print (rs :: [Point])
  cs <- callP getService "test" "counter" $ zip ([3,4,5,6] :: [Int]) ([1..] :: [Int])
  print (cs :: [Int])
