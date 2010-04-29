-- | Testing RPC methods are defined here
-- These functions will be run only in `server' (Test.hs), `client'
-- (TestCall.hs) uses only their names and types.
module Methods where

import Control.Concurrent
import System.Directory
import Codec.Binary.UTF8.String

import TestTypes

double :: State -> Point -> IO Point
double s (Point x y) = do
  print s
  return $ Point (x*2) (y*2)

mySum :: State -> [Double] -> IO Double
mySum s lst = return $ sum lst

counter :: State -> (Int,Int) -> IO Int
counter s (k,d) = do
    mapM count [k..k+10]
    return (k+10)
  where
    count i = do
      putStrLn $ show d ++ ": " ++ show i
      threadDelay (d*100000)

ls :: State -> String -> IO [String]
ls s path = do
  let path' = encodeString path
  lst <- getDirectoryContents path'
  return $ map decodeString lst

