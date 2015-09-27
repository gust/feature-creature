{-# LANGUAGE OverloadedStrings          #-}

module Main where
  import qualified Config
  import Control.Monad.IO.Class  (liftIO)
  import Control.Monad.Logger    (runStderrLoggingT)
  import Database.Persist.Postgresql
  import Products.Model
  import System.Environment (getArgs)
  import qualified CLI.Base as CLI

  runApplication :: IO ()
  runApplication = do
    args <- getArgs
    case (CLI.parseCommand args) of
      Nothing    -> CLI.showUsage
      (Just cmd) -> CLI.execCommand cmd

  initDB :: IO ()
  initDB = do
    connStr <- Config.dbConnectionString
    runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigration migrateAll

  main :: IO ()
  main = initDB >> runApplication

