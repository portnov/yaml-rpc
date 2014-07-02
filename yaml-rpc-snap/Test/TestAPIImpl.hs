{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}

module Test.TestAPIImpl where

import GHC.Generics
import qualified Data.Text as T
import Data.Aeson

import Network.YAML

data User = User {fullName :: T.Text, login :: T.Text}
  deriving (Eq, Show)

deriving instance Generic User
instance FromJSON User
instance ToJSON User

data Something = Something {smthText :: T.Text, smthList :: [T.Text]}
  deriving (Eq, Show, Generic)

instance FromJSON Something
instance ToJSON Something

sayHello :: User -> IO T.Text
sayHello user = return $ "Hello, " `T.append` fullName user `T.append` "!"

testSmth :: T.Text -> Something -> IO User
testSmth t s = return $ User {login = t `T.append` smthText s, fullName = T.intercalate " " (smthList s)}

api :: API
api = $(makeAPI "http://home.iportnov.ru/test.api" [''User, ''Something] ['sayHello, 'testSmth])

$(writeAPI "test.api" "http://home.iportnov.ru/test.api" [''User, ''Something] ['sayHello, 'testSmth])
