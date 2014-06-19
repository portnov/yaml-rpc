{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Network.YAML.Caller where

import Control.Monad
import qualified Data.Text as T
import Data.Aeson
import qualified Network.Wreq as W
import Control.Lens
import Network.HTTP.Types

class Connection c where
  connectUri :: c -> String

call server method args = do
  let json = toJSON args
  putStrLn $ "Sending request: " ++ show json
  rs <- W.post (connectUri server ++ "/" ++ T.unpack method) json
  let body = rs ^. W.responseBody
  case decode body of
    Nothing -> fail $ "Cannot decode response: " ++ show body
    Just result -> return result

instance Connection String where
  connectUri url = url
  
