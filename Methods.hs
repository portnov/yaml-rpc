-- | Testing RPC methods are defined here
-- These functions will be run only in `server' (Test.hs), `client'
-- (TestCall.hs) uses only their names and types.
module Methods where

import Control.Concurrent
import System.Directory
import Codec.Binary.UTF8.String

import TestTypes

double :: Point -> IO Point
double (Point x y) = do
  print "test"
  return $ Point (x*2) (y*2)

mySum :: [Double] -> IO Double
mySum lst = return $ sum lst

counter :: (Int,Int) -> IO Int
counter (k,d) = do
    mapM count [k..k+10]
    return (k+10)
  where
    count i = do
      putStrLn $ show d ++ ": " ++ show i
      threadDelay (d*100000)

ls :: String -> IO [String]
ls path = do
  let path' = encodeString path
  lst <- getDirectoryContents path'
  return $ map decodeString lst

