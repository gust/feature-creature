{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module Users.UsersAPI
( UsersAPI
, usersAPI
, usersServer
) where

import App
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

data User = User { id :: Int
                 , fname :: Text
                 , lname :: Text
                 } deriving (Show, Eq, Generic)

instance ToJSON   User
instance FromJSON User

data OAuth = OAuth { authorizationCode :: Text
                   } deriving (Show, Eq)

instance ToJSON OAuth where
  toJSON (OAuth code) = object [ "authorizationCode" .= code ]

instance FromJSON OAuth where
  parseJSON (Object v) = OAuth <$> v .: "authorizationCode"
  parseJSON _          = mzero

type UsersAPI = "authorize" :> QueryParam "authCode" AuthorizationCode :> Get '[JSON] User

type AuthorizationCode = Text

usersServer :: ServerT UsersAPI App
usersServer = getUser

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

getUser :: Maybe AuthorizationCode -> App User
getUser _ = return $ User 1 "Bob" "Burgers"
