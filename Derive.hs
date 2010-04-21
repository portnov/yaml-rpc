{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses #-}
module Derive where

import Language.Haskell.TH
import Control.Monad
import Data.Object
import Data.Object.Yaml

import YAML
import YAMLInstances

data T1 = T1

mkList :: [Name] -> Q Exp
mkList []       = [| [] |]
mkList (v:vars) = [| (toYamlScalar $(stringE $ nameBase v), Scalar $ toYamlScalar $(varE v)): $(mkList vars) |]

getNameBase :: Name -> Name
getNameBase name = mkName $ nameBase name

consClause :: Con -> ClauseQ
consClause (NormalC name fields) =  do
    -- Name of constructor, i.e. "A". Will become string literal in generated code
    let constructorName = nameBase name

    -- Get variables for left and right side of function definition
    (pats,vars) <- genPE (length fields)

    clause [conP name pats]                                 -- (A x1 x2)
           (normalB [| Mapping [(toYamlScalar constructorName, Mapping $(mkList vars))] |]) []

consClause (RecC name fields) = do
    -- Name of constructor, i.e. "A". Will become string literal in generated code
    let constructorName = nameBase name
        names = [getNameBase name | (name, _, _) <- fields]
        pats = map varP names
    clause [conP name pats]                                 -- (A x1 x2)
           (normalB [| Mapping [(toYamlScalar constructorName, Mapping $(mkList names))] |]) []

consClause x = report True (show x) >> return undefined


deriveToYamlObject t = do
  -- Get list of constructors for type t
  TyConI (DataD _ _ _ constructors _)  <-  reify t
  convbody <- mapM consClause constructors
  return [InstanceD [] (ConT ''ConvertSuccess `AppT` ConT t `AppT` ConT ''YamlObject) [FunD 'convertSuccess convbody]]

-- | Generate n unique variables and return them in form of patterns and expressions
genPE ::  Int -> Q ([PatQ], [Name])
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, ids)

