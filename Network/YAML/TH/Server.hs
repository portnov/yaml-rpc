{-# LANGUAGE TemplateHaskell, OverloadedStrings, PatternGuards #-}

module Network.YAML.TH.Server (makeAPI, writeAPI) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Yaml

import qualified Network.YAML.API as API

method :: Name -> ExpQ
method name = lift =<< method' name

method' :: Name -> Q API.Method
method' name = do
      var <- reify name
      case var of
        VarI _ funType _ _ -> go funType
        _ -> fail $ "Name is not of variable: " ++ show name
  where
    go (AppT (ConT _) r) = do
      resType <- convertType' r
      return $ API.Method [] resType
    go (AppT (AppT ArrowT a) b) = do
      arg <- convertType' a
      API.Method args res <- go b
      return $ API.Method (arg : args) res
    go t = fail $ "Unsupported function type: " ++ show t

stringLit :: String -> ExpQ
stringLit str = return $ LitE $ StringL str

convertType :: Type -> ExpQ
convertType (TupleT 0) = [| API.TVoid |]
convertType (ConT name)
  | "String" <- nameBase name = [| API.TString |]
  | "Text" <- nameBase name = [| API.TText |]
  | "Integer" <- nameBase name = [| API.TInteger |]
  | "Double" <- nameBase name = [| API.TDouble |]
  | otherwise = [| API.THaskell $ T.pack $ $(stringLit $ nameBase name) |]
convertType (AppT ListT t) = [| API.TList $(convertType t) |]
convertType t = fail $ "Unsupported type: " ++ show t

convertType' :: Type -> Q API.Type
convertType' (TupleT 0) = return $ API.TVoid
convertType' (ConT name)
  | "String" <- nameBase name = return $ API.TString
  | "Text" <- nameBase name = return $ API.TText
  | "Integer" <- nameBase name = return $ API.TInteger
  | "Double" <- nameBase name = return $ API.TDouble
  | otherwise = return $ API.THaskell (T.pack $ nameBase name)
convertType' (AppT ListT t) = API.TList `fmap` convertType' t
convertType' t = fail $ "Unsupported type: " ++ show t

testHello :: String -> IO String
testHello name = return $ "Hello, " ++ name ++ "!"

generateMethod :: Name -> Q [Dec]
generateMethod name = do
  let cName = mkName $ nameBase name
  let c = clause [] (normalB $ method name) []
  sequence [
    sigD cName [t| API.Method |],
    funD cName [c] ]

convertFields :: Con -> Q (M.Map T.Text API.Type)
convertFields (RecC name fs) = do
  let names = [T.pack (nameBase name) | (name, _, _) <- fs]
      types = [t | (_, _, t) <- fs]
  types' <- forM types $ \t -> convertType' t
  return $ M.fromList $ zip names types'

generateType :: Name -> ExpQ
generateType name = lift =<< generateType' name 

generateType' :: Name -> Q API.Type
generateType' name = do
  TyConI (DataD _ _ _ [constructor] _)  <-  reify name
  fields <- convertFields constructor
  return $ API.TUser fields

-- | Generate API description. Returned expression is of API type.
makeAPI :: T.Text  -- ^ Service identification URI
        -> [Name]  -- ^ List of exposed data type names
        -> [Name]  -- ^ List of exposed method names
        -> ExpQ
makeAPI uri typeNames methodNames = do
  types <- mapM generateType typeNames
  tlist <- forM (zip typeNames types) $ \(n,t) -> [| ( $(stringLit $ nameBase n), $(return t) ) |]
  typesMap <- [| M.fromList $(return $ ListE tlist) |]
  methods <- mapM method methodNames
  mlist <- forM (zip methodNames methods) $ \(n,m) -> [| ( $(stringLit $ nameBase n), $(return m) ) |]
  methodsMap <- [| M.fromList $(return $ ListE mlist) |]
  [| API.API {
       API.apiUri = $(lift uri),
       API.apiTypes = $(return typesMap),
       API.apiMethods = $(return methodsMap)
       } |]

-- | Write API description to file.
writeAPI :: FilePath  -- ^ File to write to
         -> T.Text    -- ^ Service identification URI
         -> [Name]    -- ^ List of exposed data type names
         -> [Name]    -- ^ List of exposed method names
         -> Q [Dec]
writeAPI path uri typeNames methodNames = do
  types <- mapM generateType' typeNames
  let typesMap = M.fromList [(T.pack $ nameBase n, t) | (n, t) <- zip typeNames types]
  methods <- mapM method' methodNames
  let methodsMap = M.fromList [(T.pack $ nameBase n, m) | (n, m) <- zip methodNames methods]
  let api = API.API {
              API.apiUri = uri,
              API.apiTypes = typesMap,
              API.apiMethods = methodsMap }
  runIO $ B.writeFile path $ encode api
  return []

