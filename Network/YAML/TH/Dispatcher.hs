{-# LANGUAGE TemplateHaskell, OverloadedStrings, PatternGuards, FlexibleInstances, MultiParamTypeClasses #-}

module Network.YAML.TH.Dispatcher
  (ValueFn, ToValueFn (..), Dispatcher, generateDispatcherT, generateDispatcher
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Language.Haskell.TH.Lift

import Network.YAML.API

type ValueFn m = Value -> m Value

-- | Dispatcher function gets method name and returns corresponding function, or Nothing if there is no such method.
type Dispatcher m = T.Text -> Maybe (ValueFn m)

-- | Only functions of this class can be exposed
class ToValueFn m f where
  toValueFn :: f -> ValueFn m

instance (ToJSON y, MonadIO m) => ToValueFn m (m y) where
  toValueFn fn = \rq -> do
    case rq of
      Array v -> case V.toList v of
                   [] -> do
                         y <- fn
                         return $ toJSON y
                   _ -> fail $ "Invalid number of arguments"
      _ -> fail $ "Invalid request format: " ++ show rq

instance (Monad m, FromJSON x, ToValueFn m f) => ToValueFn m (x -> f) where
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

-- | Generate dispatcher function. This will generate function called @dispatcher@.
generateDispatcherT :: Name -> API -> Q [Dec]
generateDispatcherT m (API _ _ methods) = do
    method <- newName "method"
    let c = clause [varP method] (normalB $ go method $ M.assocs methods) []
    cName <- newName "dispatcher"
    sequence [
      sigD cName [t| Dispatcher $(return $ ConT m) |],
      funD cName [c] ]
  where
    go _ [] = [| Nothing |]
    go method ((methodName, m): ms) = do
      let nameStr = T.unpack methodName
      let name = mkName nameStr 
      let other = go method ms
      [| if $(varE method) == $(return $ LitE $ StringL nameStr)
           then Just $ toValueFn $(varE name)
           else $(other) |]

generateDispatcher :: API -> Q [Dec]
generateDispatcher api = generateDispatcherT (mkName "IO") api

