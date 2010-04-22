{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.YAML.Derive where

import Language.Haskell.TH
import Control.Monad
import Data.Maybe
import Data.Default
import Data.Object
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

import Network.YAML.Base
import Network.YAML.Instances

data T1 = T1

mkList :: [Name] -> Q Exp
mkList []       = [| [] |]
mkList (v:vars) = [| (toYamlScalar $(stringOfName v), Scalar $ toYamlScalar $(varE v)): $(mkList vars) |]

getNameBase :: Name -> Name
getNameBase name = mkName $ nameBase name

stringOfName :: Name -> ExpQ
stringOfName n = sigE (stringE $ nameBase n) [t| BS.ByteString |]

nameE :: Name -> ExpQ
nameE name = varE $ getNameBase name

consClause :: Con -> ClauseQ
consClause (NormalC name fields) =  do
    -- Name of constructor, i.e. "A". Will become string literal in generated code
    let constructorName = nameBase name

    -- Get variables for left and right side of function definition
    (pats,vars) <- genPE (length fields)

    clause [conP name pats]                                 -- (A x1 x2)
           (normalB [| Mapping [(toYamlScalar (BS.pack constructorName), Mapping $(mkList vars))] |]) []

consClause (RecC name fields) = do
    -- Name of constructor, i.e. "A". Will become string literal in generated code
    let constructorName = nameBase name
        names = [getNameBase name | (name, _, _) <- fields]
        pats = map varP names
    clause [conP name pats]                                 -- (A x1 x2)
           (normalB [| Mapping [(toYamlScalar (BS.pack constructorName), Mapping $(mkList names))] |]) []

consClause x = report True (show x) >> return undefined

genFromClause cName names= do
    obj <- newName "obj"
    let guard = [| getFirstKey $(varE obj) == (BS.pack cName) |]
        body = foldl appE (conE $ mkName cName) $ map (getAttr' cName obj) $ map getNameBase names
    clause [varP obj]
        (guardedB [normalGE guard body]) []
  where
    getAttr' c obj n = [| fromMaybe def $ getSubKey (BS.pack c) $(stringOfName n) $(varE obj) |]

fromClause :: Con -> ClauseQ
fromClause (RecC name fields) = do
    let constructorName = nameBase name
        names = [getNameBase name | (name, _, _) <- fields]
    genFromClause constructorName names

fromClause (NormalC name fields) = do
    let constructorName = nameBase name
    (_,names) <- genPE (length fields)
    genFromClause constructorName names

deriveToYamlObject :: Name -> Q [Dec]
deriveToYamlObject t = do
  -- Get list of constructors for type t
  TyConI (DataD _ _ _ constructors _)  <-  reify t
  convbody <- mapM consClause constructors
  return [InstanceD [] (ConT ''ConvertSuccess `AppT` ConT t `AppT` ConT ''YamlObject) [FunD 'convertSuccess convbody]]

deriveFromYamlObject :: Name -> Q [Dec]
deriveFromYamlObject t = do
  TyConI (DataD _ _ _ constructors _)  <-  reify t
  body <- mapM fromClause constructors
  return [InstanceD [] (ConT ''ConvertSuccess `AppT` ConT ''YamlObject `AppT` ConT t) [FunD 'convertSuccess body]]

deriveIsYamlObject :: Name -> Q [Dec]
deriveIsYamlObject t = do
  [i1] <- deriveToYamlObject t
  [i2] <- deriveFromYamlObject t
  let i3 = InstanceD [] (ConT ''IsYamlObject `AppT` ConT t) []
  return [i1,i2,i3]

defaultClause :: Con -> ClauseQ
defaultClause (RecC name fields) = do
  let defs = replicate (length fields) (varE $ mkName "def")
      body = foldl appE (conE name) defs
  clause [] (normalB body) []
defaultClause (NormalC name fields) = do
  let defs = replicate (length fields) (varE $ mkName "def")
      body = foldl appE (conE name) defs
  clause [] (normalB body) []

deriveDefault :: Name -> Q [Dec]
deriveDefault t = do
  TyConI (DataD _ _ _ constructors _)  <-  reify t
  body <- defaultClause (head constructors)
  return [InstanceD [] (ConT ''Default `AppT` ConT t) [FunD 'def [body]]]

-- | Generate n unique variables and return them in form of patterns and expressions
genPE ::  Int -> Q ([PatQ], [Name])
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, ids)

