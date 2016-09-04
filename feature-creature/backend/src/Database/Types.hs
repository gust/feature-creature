{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Database.Types
( DBAccess (..)
, db
) where

import Control.Monad.Except
import Database.Persist.Postgresql
import Models (Product)

data DBAccess m = DBAccess { runDb :: forall a . m a -> IO a
                           , selectProducts :: m [Entity Product]
                           , insertProduct :: Product -> m (Key Product)
                           }

db :: ConnectionPool -> DBAccess (SqlPersistT IO)
db pool = DBAccess { runDb = runDb' pool
                   , selectProducts = selectProducts'
                   , insertProduct = insertProduct'
                   }
  where
    runDb' :: ConnectionPool -> SqlPersistT IO a -> IO a
    runDb' conn query = liftIO (runSqlPool query conn)

    selectProducts' :: SqlPersistT IO [Entity Product]
    selectProducts' = selectList ([] :: [Filter Product]) []

    insertProduct' :: Product -> SqlPersistT IO (Key Product)
    insertProduct' = insert
