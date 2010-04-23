{-# LANGUAGE TemplateHaskell #-}

module Network.YAML.WrapMethods
  (remote, declareRules)
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

rulePair :: Name -> ExpQ
rulePair name = [| ($(stringOfName name), yamlMethod $(varE name)) |]

mkList :: [Exp] -> ExpQ
mkList [] = [| [] |]
mkList (e:es) = [| $(return e): $(mkList es) |]

declareRules :: [Name] -> Q [Dec]
declareRules names = do
  pairs <- mapM rulePair names
  let body = [| mkRules $(mkList pairs) |]
      c = clause [] (normalB body) []
  sequence [
    funD (mkName "dispatchingRules") [c]]

