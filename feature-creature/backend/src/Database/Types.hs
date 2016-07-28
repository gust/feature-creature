{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Database.Types
( DBAccess (..)
, db
) where

import Control.Monad.Except
import Database.Persist.Postgresql

data DBAccess m = DBAccess { runDb :: forall a . m a -> IO a
                           }

db :: ConnectionPool -> DBAccess (SqlPersistT IO)
db pool = DBAccess { runDb = runDb' pool }
  where
    runDb' :: ConnectionPool -> SqlPersistT IO a -> IO a
    runDb' conn query = liftIO (runSqlPool query conn)
