{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Database.Persist.TH
import Data.Time.Clock (UTCTime)
import Database.Persist.Postgresql (ConnectionPool, runSqlPool, runMigration)
import Database.Persist.Sql(toSqlKey, SqlBackend, ToBackendKey, Key(..))
import Retry (withRetry)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Product json sql=products
    created UTCTime default=now()
    deriving Show
|]

runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  withRetry (runSqlPool (runMigration migrateAll) pool)

toKey :: (Integral i, ToBackendKey SqlBackend record) => i -> Key record
toKey = toSqlKey . fromIntegral
