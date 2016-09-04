{-# LANGUAGE FlexibleContexts #-}

module Users.Controller
  ( UserIndex
  , UserCreate
  , UserShow
  , UserDelete
  , UserUpdate
  , usersIndex
  , usersShow
  , usersCreate
  , usersUpdate
  , usersDelete
  ) where

import Users (User (..), UserForm (..), formValidationErrors, hasValidationErrors)
import App (AppT, AppConfig (..))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Data.Time.Clock as Clock
import qualified Database.Persist.Postgresql as DB
import Errors (AppError (..), raiseAppError)
import qualified Models as M
import Servant
import qualified Users.Query as Q

type UserIndex  = Get '[JSON] [User]
type UserCreate = ReqBody '[JSON] UserForm :> Post '[JSON] User
type UserShow   = Capture "auth-id" Text :> Get '[JSON] User
type UserDelete = Capture "auth-id" Text :> Delete '[JSON] ()
type UserUpdate = Capture "auth-id" Text :> ReqBody '[JSON] UserForm :> Patch '[JSON] User

usersIndex :: AppT [User]
usersIndex = ask >>= \AppConfig{..} ->
  fmap (map Q.toApiUser) (Q.findAll getDB)

usersShow :: Text -> AppT User
usersShow authId = ask >>= \AppConfig{..} -> do
  mUser <- Q.find authId getDB
  case mUser of
    Just userEnt -> return $ Q.toApiUser' (DB.entityKey userEnt) (DB.entityVal userEnt)
    Nothing      -> raiseAppError ResourceNotFound

usersCreate :: UserForm -> AppT User
usersCreate userForm =
  if hasValidationErrors userForm then
    raiseAppError $ BadRequest (formValidationErrors userForm)
  else
    createNewUser userForm

createNewUser :: UserForm -> AppT User
createNewUser (UserForm _ authProviderId fname lname) = ask >>= \AppConfig{..} -> do
  now    <- liftIO Clock.getCurrentTime
  userId <- Q.create (M.User authProviderId fname lname now) getDB
  return $ User userId authProviderId fname lname

usersUpdate :: Text -> UserForm -> AppT User
usersUpdate authId userForm =
  if hasValidationErrors userForm then
    raiseAppError $ BadRequest (formValidationErrors userForm)
  else
    updateUser authId userForm

updateUser :: Text -> UserForm -> AppT User
updateUser authId (UserForm _ _ fname lname) = ask >>= \AppConfig{..} -> do
  mUser <- Q.find authId getDB
  case mUser of
    Just userEnt -> do
      let updatedUser = M.User authId fname lname (M.userCreated (DB.entityVal userEnt))
      Q.update (DB.entityKey userEnt) updatedUser getDB
      return $ Q.toApiUser' (DB.entityKey $ userEnt) updatedUser
    Nothing      -> raiseAppError ResourceNotFound

usersDelete :: Text -> AppT ()
usersDelete userId = ask >>= \AppConfig{..} ->
  Q.delete userId getDB
