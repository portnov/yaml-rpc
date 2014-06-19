{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Network.YAML.API where

import Control.Monad
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

data Type =
    TVoid
  | TString
  | TText
  | TInteger
  | TDouble
  | TUser (M.Map T.Text Type)
  | THaskell T.Text
  deriving (Eq, Show)

$(deriveLift ''Type)

data Method = Method {
    methodArgs :: M.Map T.Text Type
  , methodReturnType :: Type
  } deriving (Eq, Show)

$(deriveLift ''Method)

data API = API {
    apiUri :: T.Text
  , apiTypes :: M.Map T.Text Type
  , apiMethods :: M.Map T.Text Method
  } deriving (Eq, Show)

$(deriveLift ''API)

instance FromJSON Type where
  parseJSON (String "Void") = return TVoid
  parseJSON (String "String") = return TString
  parseJSON (String "Text") = return TText
  parseJSON (String "Integer") = return TInteger
  parseJSON (String "Double") = return TDouble
  parseJSON (String text) = return (THaskell text)
  parseJSON x@(Object v) = do
      typeFields <- parseJSON x
      return $ TUser typeFields
  parseJSON x = fail $ "Invalid type description: " ++ show x

instance FromJSON Method where
  parseJSON (Object v) = do
    returnType <- v .:? "return" .!= TVoid
    args <- parseJSON $ Object $ H.delete "return" v
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
  let args' = M.map (resolveType types) args
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
  toJSON (TUser fields) = Object $ H.fromList [(name, toJSON t) | (name, t) <- M.assocs fields]
  toJSON (THaskell name) = String name

instance ToJSON Method where
  toJSON (Method args returnType) =
    object $ [name .= t | (name, t) <- M.assocs args] ++ ["return" .= returnType]

instance ToJSON API where
  toJSON (API uri types methods) =
    object ["uri" .= uri,
            "types" .= types,
            "methods" .= methods]


testAPI :: API
testAPI = API {
    apiUri = "http://home.iportnov.ru/test.api"
  , apiTypes = M.fromList [("User",TUser (M.fromList [("fullName",TText),("login",TText)]))]
  , apiMethods = M.fromList [("sayHello", Method {methodArgs = M.fromList [("user",THaskell "User")],
                                                methodReturnType = TText})]
  }

readAPI :: FilePath -> TH.ExpQ
readAPI path = do
  x <- TH.runIO $ (decodeFile path :: IO (Maybe API))
  case x of
    Nothing -> fail $ "Cannot read API from " ++ path
    Just api -> lift api

