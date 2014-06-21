{-# LANGUAGE TemplateHaskell, OverloadedStrings, PatternGuards #-}

module Network.YAML.TH.Client (generateAPI, useAPI) where

import Control.Monad
import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import Data.Yaml

import qualified Network.YAML.API as API
import Network.YAML.Caller

-- | Generate data types and wrapper methods declarations from API description, read from file
useAPI :: FilePath -> Q [Dec]
useAPI path = do
  x <- runIO $ decodeFileEither path
  case x of
    Left err -> fail $ "Cannot parse API description file " ++ path ++ ": " ++ show err
    Right api -> generateAPI api

-- | Generate data types and wrapper methods declarations from API description
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
convertType (API.TList t) = AppT ListT `fmap` convertType t
convertType (API.TUser _) = fail $ "User-defined types cannot be nested"

methodType :: API.Method -> Q Type
methodType (API.Method methodArgs methodRet) = go (methodArgs ++ [methodRet])
  where
    go [r] = do
      r' <- convertType r
      return $ AppT (ConT $ mkName "IO") r'
    go (t: ts) = do
      result <- go ts
      t' <- convertType t
      return $ AppT (AppT ArrowT t') result

generateMethod :: (T.Text, API.Method) -> Q [Dec]
generateMethod (text, method) = do
  srv <- newName "srv"
  argNames <- forM (zip [0..] $ API.methodArgs method) $ \(i, _) ->
                  newName $ "arg" ++ show i
  let argNamesT = map (T.pack . nameBase) argNames
  let argPatterns = map varP argNames
  args <- forM argNames $ \name -> [| toJSON $(varE name) |]
  let c = clause (varP srv: argPatterns) (normalB [| call $(varE srv) $(lift text) $(return $ ListE args) |]) []
      cName = mkName $ T.unpack text
  mt <- methodType method
  sequence [
    sigD cName [t| (Connection c) => c -> $(return mt) |],
    funD cName [c] ]


