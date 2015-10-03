module Database where
  import Config as Cfg
  import Control.Monad.IO.Class (liftIO)
  import Control.Monad.Logger as ML (runStderrLoggingT) 
  import qualified Database.Persist.Postgresql as DB
  import qualified Models

  runDB :: DB.SqlPersistM b -> IO b
  runDB query = do
    connStr <- Cfg.dbConnectionString
    ML.runStderrLoggingT $
      DB.withPostgresqlPool connStr 1 $
        \pool -> liftIO $ DB.runSqlPersistMPool query pool

  initDB :: IO ()
  initDB = runDB $ DB.runMigration Models.migrateAll
