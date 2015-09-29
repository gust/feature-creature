{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where
  import Config as Cfg
  import Control.Applicative (Applicative)
  import Control.Monad.IO.Class (MonadIO, liftIO)
  import Control.Monad.Logger as ML (runStderrLoggingT) 
  import Control.Monad.Reader (MonadReader, ReaderT, asks)
  import Control.Monad.Trans.Class (MonadTrans, lift)
  import qualified Database.Persist.Postgresql as DB
  import qualified Models

  data Config = Config
    { pool :: DB.ConnectionPool
    }

  newtype ConfigM a = ConfigM
    { runConfigM :: ReaderT Config IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

  runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
    DB.SqlPersistT IO a -> t ConfigM a
  runDB query = do
    dbPool <- lift (asks pool)
    liftIO (DB.runSqlPool query dbPool)

  makePool :: IO DB.ConnectionPool
  makePool = do
    connStr <- Cfg.dbConnectionString
    ML.runStderrLoggingT $ DB.createPostgresqlPool connStr 1

  initDB :: IO ()
  initDB = do
    dbPool <- makePool
    DB.runSqlPool (DB.runMigration Models.migrateAll) dbPool
