{-# LANGUAGE TemplateHaskell #-}

module Network.YAML.WrapMethods
  (remote, remote', declareRules, declareRulesWithArg)
  where

import Language.Haskell.TH
import Control.Monad
import Data.Char (toUpper)
import Data.Object.Yaml
import qualified Data.ByteString.Char8 as BS

import Network.YAML.Base
import Network.YAML.Caller
import Network.YAML.Derive
import Network.YAML.Instances
import Network.YAML.Dispatcher

-- | Declare given function as remote method. 
-- This creates a function with same name as given (so qualified name must be
-- used as argument), and almost same behaivour. Difference is that newly
-- declared function takes pair (host name, port number) as first argument.
remote :: Name -> Q [Dec]
remote name = do
  srv <- newName "srv"
  let c = clause [varP srv] (normalB [| call $(varE srv) $(stringOfName name) |]) []
      cName = mkName $ nameBase name
  (VarI _ tp _ _) <- reify name
  let AppT (AppT ArrowT a) ioB = tp
  sequence [
    sigD cName [t| (BS.ByteString, Int) -> $(return a) -> $(return ioB) |],
    funD cName [c]]

remote' :: Name -> Q [Dec]
remote' name = do
  srv <- newName "srv"
  let c = clause [varP srv] (normalB [| call $(varE srv) $(stringOfName name) |]) []
      cName = mkName $ nameBase name
  (VarI _ tp _ _) <- reify name
  let AppT (AppT ArrowT _) (AppT (AppT ArrowT a) ioB) = tp
  sequence [
    sigD cName [t| (BS.ByteString, Int) -> $(return a) -> $(return ioB) |],
    funD cName [c]]

rulePair :: Name -> ExpQ
rulePair name = [| ($(stringOfName name), yamlMethod $(varE name)) |]

rulePairWithArg :: Name -> Name -> ExpQ
rulePairWithArg arg name = [| ($(stringOfName name), yamlMethod ($(varE name) $(varE arg))) |]

mkList :: [Exp] -> ExpQ
mkList [] = [| [] |]
mkList (e:es) = [| $(return e): $(mkList es) |]

-- | Declare dispatching rules for given list of functions. 
-- Map with rules will be called dispatchingRules.
-- For each given function RPC method with same name will be declared.
declareRules :: [Name] -> Q [Dec]
declareRules names = do
  pairs <- mapM rulePair names
  let body = [| mkRules $(mkList pairs) |]
      c = clause [] (normalB body) []
  sequence [
    funD (mkName "dispatchingRules") [c]]

-- | Similar, but pass given arg as first argument to all functions
declareRulesWithArg :: Name -> [Name] -> Q [Dec]
declareRulesWithArg arg names = do
  pairs <- mapM (rulePairWithArg arg) names
  let body = [| mkRules $(mkList pairs) |]
      c = clause [] (normalB body) []
  sequence [
    funD (mkName "dispatchingRules") [c]]
