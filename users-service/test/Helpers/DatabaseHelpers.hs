module Helpers.DatabaseHelpers
  ( setupTestDatabase
  , truncateDatabase
  ) where

import Models (runMigrations)
import qualified Database.Persist.Postgresql as DB

setupTestDatabase :: DB.ConnectionPool -> IO ()
setupTestDatabase pool = do
  runMigrations pool
  truncateDatabase pool

truncateDatabase :: DB.ConnectionPool -> IO ()
truncateDatabase = DB.runSqlPool truncateQuery
  where
    truncateQuery = DB.rawExecute "TRUNCATE TABLE users RESTART IDENTITY CASCADE;" []
