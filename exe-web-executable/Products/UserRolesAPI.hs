{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Products.UserRolesAPI
( UserRolesAPI
, CreateUserRolesAPI
, EditUserRolesAPI
, RemoveUserRoleAPI
, APIUserRole (..)
, createUserRole
, editUserRole
, removeUserRole
, productsUserRoles
) where

import App
import AppConfig (DBConfig (..), getDBConfig)
import Control.Monad.Reader
import Control.Monad.Trans.Either (left)
import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text          as T
import Data.Time.Clock as Clock
import Database.Types (runPool)
import Models
import qualified Products.Product   as P
import Servant
import qualified UserRoles.UserRole as UR


type UserRolesAPI       = "user-roles" :> Get '[JSON] [APIUserRole]
type CreateUserRolesAPI = "user-roles" :> ReqBody '[JSON] APIUserRole :> Post '[JSON] APIUserRole
type EditUserRolesAPI   = "user-roles" :> Capture "id" Int64 :> ReqBody '[JSON] APIUserRole :> Put '[JSON] APIUserRole
type RemoveUserRoleAPI  = "user-roles" :> Capture "id" Int64 :> Delete '[JSON] ()

data APIUserRole = APIUserRole { userRoleID   :: Maybe Int64
                               , productID    :: Maybe ProductId
                               , title        :: T.Text
                               , description  :: T.Text
                               } deriving (Show)

instance ToJSON APIUserRole where
  toJSON (APIUserRole termID prodID termTitle termDescription) =
    object [ "id"          .= termID
           , "productID"   .= prodID
           , "title"       .= termTitle
           , "description" .= termDescription
           ]

instance FromJSON APIUserRole where
  parseJSON (Object v) = APIUserRole <$>
                        v .:? "id" <*>
                        v .:? "productID" <*>
                        v .: "title" <*>
                        v .: "description"
  parseJSON _          = mzero

createUserRole :: P.ProductID -> APIUserRole -> App APIUserRole
createUserRole pID (APIUserRole _ _ t d) = do
  dbConfig <- reader getDBConfig
  utcTime  <- liftIO $ Clock.getCurrentTime
  roleID   <- liftIO $ runReaderT (runPool (UR.createUserRole (UserRole (toKey pID) t d utcTime))) (getPool dbConfig)
  return $ APIUserRole { userRoleID  = Just roleID
                       , productID   = Just (toKey pID)
                       , title       = t
                       , description = d
                       }

editUserRole :: P.ProductID -> Int64 -> APIUserRole -> App APIUserRole
editUserRole pID urID (APIUserRole _ _ t d) = do
  dbConfig <- reader getDBConfig
  userRole <- liftIO $ runReaderT (runPool (UR.findUserRole (toKey urID))) (getPool dbConfig)
  case userRole of
    Nothing -> lift $ left $ err404
    (Just ur) ->
      (liftIO $ runReaderT (runPool (UR.updateUserRole (toKey urID) ur)) (getPool dbConfig))
        >> (return $ APIUserRole { userRoleID = Just (urID)
                                 , productID    = Just (toKey pID)
                                 , title        = t
                                 , description  = d
                                 })

removeUserRole :: P.ProductID -> Int64 -> App ()
removeUserRole pID urID = do
  dbConfig <- reader getDBConfig
  liftIO $ runReaderT (runPool (UR.removeUserRole (toKey pID) (toKey urID))) (getPool dbConfig)

productsUserRoles :: P.ProductID -> App [APIUserRole]
productsUserRoles prodID = do
  dbConfig <- reader getDBConfig
  userRoles <- liftIO $ runReaderT (runPool (UR.findByProductId (toKey prodID))) (getPool dbConfig)
  return $ map toUserRole userRoles
    where
      toUserRole dbUserRole = do
        let dbTerm   = UR.toUserRole dbUserRole
        let dbTermID = UR.toUserRoleID dbUserRole
        APIUserRole { userRoleID   = Just dbTermID
                    , productID    = Just $ userRoleProductId dbTerm
                    , title        = userRoleTitle dbTerm
                    , description  = userRoleDescription dbTerm
                    }

