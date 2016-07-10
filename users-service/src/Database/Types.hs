{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Database.Types
( DBAccess (..)
, db
) where

import Control.Monad.Except
import Database.Persist.Postgresql
import Models

data DBAccess m = DBAccess { runDb    :: forall a . m a -> IO a
                           , selectUsers :: m [Entity User]
                           , getUser    :: Unique User -> m (Maybe (Entity User))
                           , insertUser :: User -> m (Key User)
                           , deleteUser :: Unique User -> m ()
                           , updateUser :: Key User -> User -> m ()
                           }

db :: ConnectionPool -> DBAccess (SqlPersistT IO)
db pool = DBAccess { runDb = runDb' pool
                   , selectUsers = selectUsers'
                   , getUser = getUser'
                   , insertUser = insertUser'
                   , deleteUser = deleteUser'
                   , updateUser = updateUser'
                   }
  where
    runDb' :: ConnectionPool -> SqlPersistT IO a -> IO a
    runDb' conn query = liftIO (runSqlPool query conn)

    selectUsers' :: SqlPersistT IO [Entity User]
    selectUsers' = selectList ([] :: [Filter User]) []

    getUser' :: Unique User -> SqlPersistT IO (Maybe (Entity User))
    getUser' = getBy

    insertUser' :: User -> SqlPersistT IO (Key User)
    insertUser' = insert

    deleteUser' :: Unique User -> SqlPersistT IO ()
    deleteUser' = deleteBy

    updateUser' :: Key User -> User -> SqlPersistT IO ()
    updateUser' = replace
