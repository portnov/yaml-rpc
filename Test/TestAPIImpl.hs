{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}

module Test.TestAPIImpl where

import GHC.Generics
import qualified Data.Text as T
import Data.Aeson

import Network.YAML.API
import qualified Network.YAML.TH.Server as S
import Network.YAML.TH.Dispatcher

data User = User {fullName :: T.Text, login :: T.Text}
  deriving (Eq, Show)

deriving instance Generic User
instance FromJSON User
instance ToJSON User

sayHello :: User -> IO T.Text
sayHello user = return $ "Hello, " `T.append` fullName user `T.append` "!"

api :: API
api = $(S.makeAPI "http://home.iportnov.ru/test.api" [''User] ['sayHello])
