-- | Testing RPC methods are defined here
module Methods where

import Control.Concurrent

import TestTypes

double :: Point -> IO Point
double (Point x y) = return $ Point (x*2) (y*2)

mySum :: [Double] -> IO Double
mySum = return . sum

counter :: (Int,Int) -> IO Int
counter (k,d) = do
    mapM count [k..k+10]
    return (k+10)
  where
    count i = do
      putStrLn $ show d ++ ": " ++ show i
      threadDelay (d*100000)

