{-# LANGUAGE TemplateHaskell, OverloadedStrings, PatternGuards #-}

module Network.YAML.TH.Client (generateAPI) where

import Control.Monad
import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift
import qualified Data.Text as T
import Data.Aeson

import qualified Network.YAML.API as API
import Network.YAML.Caller

generateAPI :: API.API -> Q [Dec]
generateAPI (API.API _ types methods) = do
  ts <- mapM generateType $ M.assocs types
  ms <- mapM generateMethod $ M.assocs methods
  return $ concat ts ++ concat ms

generateType :: (T.Text, API.Type) -> Q [Dec]
generateType (text, API.TUser fields) = do
  let name = mkName $ T.unpack text
  fields' <- mapM convertField $ M.assocs fields
  let constructor = RecC name fields'
  return [ DataD [] name [] [constructor] [] ]
generateType (_,_) = return []

convertField :: (T.Text, API.Type) -> Q VarStrictType
convertField (text, t) = do
  t' <- convertType t
  return (mkName $ T.unpack text, NotStrict, t')

convertType :: API.Type -> Q Type
convertType API.TVoid = return $ TupleT 0
convertType API.TString = return $ ConT $ mkName "String"
convertType API.TText = return $ ConT $ mkName "Text"
convertType API.TInteger = return $ ConT $ mkName "Integer"
convertType API.TDouble = return $ ConT $ mkName "Double"
convertType (API.THaskell name) = return $ ConT $ mkName (T.unpack name)
convertType (API.TUser _) = fail $ "User-defined types cannot be nested"

methodType :: API.Method -> Q Type
methodType (API.Method methodArgs methodRet)
  | M.null methodArgs = do
      r <- convertType methodRet
      return $ AppT (ConT $ mkName "IO") r
  | otherwise = do
      r <- convertType methodRet
      args <- convertMethodArgTypes $ map snd $ M.assocs methodArgs
      return $ AppT (AppT ArrowT args) (AppT (ConT $ mkName "IO") r)

convertMethodArgTypes :: [API.Type] -> Q Type
convertMethodArgTypes [] = fail $ "Impossible: convertMethodArgTypes: empty list"
convertMethodArgTypes [t] = convertType t
convertMethodArgTypes (t:ts) = do
  res <- convertMethodArgTypes ts
  x <- convertType t
  return $ AppT (AppT ArrowT x) res

generateMethod :: (T.Text, API.Method) -> Q [Dec]
generateMethod (text, method) = do
  srv <- newName "srv"
  let argNamesT = M.keys $ API.methodArgs method
  let argNames = map (\n -> mkName (T.unpack n)) argNamesT
  let argPatterns = map varP argNames
  pairs <- forM (zip argNamesT argNames) $ \(nameT, name) ->
             [| $(lift nameT) .= $(varE name) |]
  let args = [| object $(return $ ListE pairs) |]
  let c = clause (varP srv: argPatterns) (normalB [| call $(varE srv) $(lift text) $(args) |]) []
      cName = mkName $ T.unpack text
  mt <- methodType method
  sequence [
    sigD cName [t| (Connection c) => c -> $(return mt) |],
    funD cName [c] ]


