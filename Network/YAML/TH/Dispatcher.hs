{-# LANGUAGE TemplateHaskell, OverloadedStrings, PatternGuards, FlexibleInstances #-}

module Network.YAML.TH.Dispatcher where

import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Language.Haskell.TH.Lift

import Network.YAML.API

type ValueFn = Value -> IO Value

type Dispatcher = T.Text -> Maybe ValueFn

class ToValueFn m where
  toValueFn :: [T.Text] -> m -> ValueFn

instance (FromJSON x, ToJSON y) => ToValueFn (x -> IO y) where
  toValueFn [name] fn = \rq -> do
    case rq of
      Object v -> case H.lookup name v of
                    Nothing -> fail $ "Argument not found: " ++ T.unpack name
                    Just arg ->
                      case fromJSON arg of
                        Error str -> fail $ "Request parsing error: " ++ str
                        Success x -> do
                            y <- fn x
                            return $ toJSON y

generateDispatcher :: API -> Q [Dec]
generateDispatcher (API _ _ methods) = do
    method <- newName "method"
    let c = clause [varP method] (normalB $ go method $ M.assocs methods) []
    cName <- newName "dispatcher"
    sequence [
      sigD cName [t| Dispatcher |],
      funD cName [c] ]
  where
    go _ [] = [| Nothing |]
    go method ((methodName, m): ms) = do
      let nameStr = T.unpack methodName
      let name = mkName nameStr 
      let other = go method ms
      let argNames = M.keys $ methodArgs m
      [| if $(varE method) == $(return $ LitE $ StringL nameStr)
           then Just $ toValueFn $(lift argNames) $(varE name)
           else $(other) |]

