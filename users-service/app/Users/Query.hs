module Users.Query
  ( findAll
  , find
  , create
  , update
  , delete
  , toApiUser
  , toApiUser'
  ) where

import Control.Monad.IO.Class
import qualified Database.Persist.Postgresql as DB
import Data.Text (Text)
import Database.Types
import GHC.Int (Int64)
import Models
import qualified Users as Api

findAll :: (MonadIO app, Monad m) => DBAccess m -> app [DB.Entity User]
findAll DBAccess{..} =
  liftIO $ runDb selectUsers

find :: (MonadIO app, Monad m) => Text -> DBAccess m -> app (Maybe (DB.Entity User))
find authId DBAccess{..} =
  liftIO $ runDb (getUser (UniqueAuthProviderId authId))

create :: (MonadIO app, Monad m) => User -> DBAccess m -> app Int64
create user DBAccess{..} =
  liftIO $ DB.fromSqlKey <$> runDb (insertUser user)

update :: (MonadIO app, Monad m) => UserId -> User -> DBAccess m -> app ()
update uID user DBAccess{..} =
  liftIO $ runDb (updateUser uID user)

delete :: (MonadIO app, Monad m) => Text -> DBAccess m -> app ()
delete authId DBAccess{..} =
  liftIO $ runDb (deleteUser (UniqueAuthProviderId authId))

toApiUser :: DB.Entity User -> Api.User
toApiUser userEnt =
  Api.User
    (DB.fromSqlKey $ DB.entityKey userEnt)
    (userAuthProviderId $ DB.entityVal userEnt)
    (userEmail $ DB.entityVal userEnt)
    (userName $ DB.entityVal userEnt)

toApiUser' :: UserId -> User -> Api.User
toApiUser' uID (User authProviderId fname lname _) =
  Api.User (DB.fromSqlKey uID) authProviderId fname lname
