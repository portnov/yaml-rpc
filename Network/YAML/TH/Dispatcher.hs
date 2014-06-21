{-# LANGUAGE TemplateHaskell, OverloadedStrings, PatternGuards, FlexibleInstances #-}

module Network.YAML.TH.Dispatcher where

import Control.Monad
import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Language.Haskell.TH.Lift

import Network.YAML.API

type ValueFn = Value -> IO Value

type Dispatcher = T.Text -> Maybe ValueFn

class ToValueFn m where
  toValueFn :: m -> ValueFn

instance (ToJSON y) => ToValueFn (IO y) where
  toValueFn fn = \rq -> do
    case rq of
      Array v -> case V.toList v of
                   [] -> do
                         y <- fn
                         return $ toJSON y
                   _ -> fail $ "Invalid number of arguments"
      _ -> fail $ "Invalid request format: " ++ show rq

instance (FromJSON x, ToValueFn f) => ToValueFn (x -> f) where
  toValueFn fn = \rq -> do
    case rq of
      Array v -> case V.toList v of
                   (arg:_) ->
                     case fromJSON arg of
                        Error str -> fail $ "Request parsing error: " ++ str
                        Success x -> do
                          toValueFn (fn x) $ Array $ V.tail v
                   _ -> fail $ "Invalid number of arguments"
      _ -> fail $ "Invalid request format: " ++ show rq

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
      argNames <- forM (zip [0..] $ methodArgs m) $ \(i, _) ->
                      newName $ "arg" ++ show i
      [| if $(varE method) == $(return $ LitE $ StringL nameStr)
           then Just $ toValueFn $(varE name)
           else $(other) |]

