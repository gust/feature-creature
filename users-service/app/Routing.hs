module Routing
  ( API
  , api
  , server
  ) where

import App (AppT)
import qualified Documentation.Controller as Docs
import qualified Users.Controller as Users
import Data.Swagger
import Servant

type API = SwaggerAPI
      :<|> UsersAPI

type SwaggerAPI = "v1" :> "docs"  :> Get '[JSON] Swagger

type UsersAPI   = "v1" :> "users" :> Users.UserIndex
             :<|> "v1" :> "users" :> Users.UserShow
             :<|> "v1" :> "users" :> Users.UserCreate
             :<|> "v1" :> "users" :> Users.UserUpdate
             :<|> "v1" :> "users" :> Users.UserDelete

server :: ServerT API AppT
server = Docs.documentationIndex usersApi
    :<|> Users.usersIndex
    :<|> Users.usersShow
    :<|> Users.usersCreate
    :<|> Users.usersUpdate
    :<|> Users.usersDelete

api :: Proxy API
api = Proxy

usersApi :: Proxy UsersAPI
usersApi = Proxy
