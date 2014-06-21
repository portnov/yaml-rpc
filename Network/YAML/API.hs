{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Network.YAML.API
  (Type (..), Method (..), API (..),
   readAPI
  ) where

import Control.Monad
import Data.Char
import Data.Yaml
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Lift

instance (Lift a, Lift b) => Lift (M.Map a b) where
  lift m = [| M.fromList $ $(lift list) |]
    where list = M.assocs m

instance Lift T.Text where
  lift text = [| T.pack $(lift str) |]
    where str = T.unpack text

-- | Data type description for API
data Type =
    TVoid        -- ^ Like (); YAML notation for it is Void.
  | TString
  | TText
  | TInteger
  | TDouble
  | TList Type   -- ^ @[Type]@. YAML notation is @List Type@.
  | TUser (M.Map T.Text Type) -- ^ User-defined record type
  | THaskell T.Text -- ^ Any Haskell type
  deriving (Eq, Show)

$(deriveLift ''Type)

-- | API method description
data Method = Method {
    methodArgs :: [Type]      -- ^ Types of method arguments
  , methodReturnType :: Type  -- ^ Method return value type
  } deriving (Eq, Show)

$(deriveLift ''Method)

-- | API description
data API = API {
    apiUri :: T.Text                    -- ^ API service identification
  , apiTypes :: M.Map T.Text Type       -- ^ Exposed data types
  , apiMethods :: M.Map T.Text Method   -- ^ Exposed methods
  } deriving (Eq, Show)

$(deriveLift ''API)

instance FromJSON Type where
  parseJSON (String "Void") = return TVoid
  parseJSON (String "String") = return TString
  parseJSON (String "Text") = return TText
  parseJSON (String "Integer") = return TInteger
  parseJSON (String "Double") = return TDouble
  parseJSON (String text) = do
      let lst = filter (not . T.null) $ T.split isSpace text
      if (length lst == 2) && (head lst == "List")
        then TList `fmap` parseJSON (String $ lst !! 1)
        else return (THaskell text)
  parseJSON x@(Object v) = do
      typeFields <- parseJSON x
      return $ TUser typeFields
  parseJSON x = fail $ "Invalid type description: " ++ show x

instance FromJSON Method where
  parseJSON (Object v) = do
    returnType <- v .:? "return" .!= TVoid
    args <- v .:? "arguments" .!= []
    return $ Method args returnType
  parseJSON x = fail $ "Invalid method description: " ++ show x

resolveType :: M.Map T.Text Type -> Type -> Type
resolveType types t@(THaskell name) =
  case M.lookup name types of
    Just result -> result
    Nothing -> t
resolveType _ t = t

resolveMethodTypes :: M.Map T.Text Type -> Method -> Method
resolveMethodTypes types (Method args returnType) =
  let args' = map (resolveType types) args
      returnType' = resolveType types returnType
  in Method args' returnType'

instance FromJSON API where
  parseJSON (Object v) = do
    uri <- v .: "uri"
    types <- v .:? "types" .!= M.empty
    methods <- v .:? "methods" .!= M.empty
    return $ API uri types methods
  parseJSON x = fail $ "Invalid API description: " ++ show x

instance ToJSON Type where
  toJSON TVoid = String "Void"
  toJSON TString = String "String"
  toJSON TText = String "Text"
  toJSON TInteger = String "Integer"
  toJSON TDouble = String "Double"
  toJSON (TList t) = case toJSON t of
                       String s -> String $ "List " `T.append` s
                       x -> error $ "Unsupported inner type for List: " ++ show x
  toJSON (TUser fields) = Object $ H.fromList [(name, toJSON t) | (name, t) <- M.assocs fields]
  toJSON (THaskell name) = String name

instance ToJSON Method where
  toJSON (Method args returnType) =
    object [
      "arguments" .= args,
      "return" .= returnType ]

instance ToJSON API where
  toJSON (API uri types methods) =
    object ["uri" .= uri,
            "types" .= types,
            "methods" .= methods]

testAPI :: API
testAPI = API {
    apiUri = "http://home.iportnov.ru/test.api"
  , apiTypes = M.fromList [("User",TUser (M.fromList [("fullName",TText),("login",TText)]))]
  , apiMethods = M.fromList [("sayHello", Method {methodArgs = [THaskell "User"],
                                                methodReturnType = TText})]
  }

-- | Read API definition from file. Returned expression is of type API.
readAPI :: FilePath -> TH.ExpQ
readAPI path = do
  x <- TH.runIO $ (decodeFile path :: IO (Maybe API))
  case x of
    Nothing -> fail $ "Cannot read API from " ++ path
    Just api -> lift api

