{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Postgresql (ConnectionPool, runSqlPool, runMigration)
import Database.Persist.Sql(toSqlKey, SqlBackend, ToBackendKey, Key(..))
import Retry (withRetry)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User json sql=users
    authProviderId Text sqltype=text
    email Text sqltype=text
    name Text sqltype=text
    created UTCTime default=now()
    UniqueAuthProviderId authProviderId
    UniqueEmail email
    deriving Show
|]

runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  withRetry (runSqlPool (runMigration migrateAll) pool)

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral
