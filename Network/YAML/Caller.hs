{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Network.YAML.Caller where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Yaml
import qualified Network.Wreq as W
import Control.Lens
import Network.HTTP.Types

-- | This class will be extended in future.
class Connection c where
  connectUri :: c -> String

-- | Call remote method
call :: (ToJSON args, Connection srv, FromJSON result, MonadIO m)
     => srv         -- ^ Server connection; in simplest case - URL of service
     -> T.Text      -- ^ Method name
     -> args        -- ^ Method arguments
     -> m result
call server method args = do
  let json = toJSON args
  rs <- liftIO $ W.post (connectUri server ++ "/" ++ T.unpack method) json
  let body = rs ^. W.responseBody
  case decode (BL.toStrict body) of
    Nothing -> fail $ "Cannot decode response: " ++ show body
    Just result -> return result

instance Connection String where
  connectUri url = url
  
