module Main where

import App (AppT, AppConfig)
import qualified App as App
import Config.Environment (Environment(..), getCurrentEnvironment)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Models as M
import Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.RequestLogger.LogEntries (logEntriesLogger)
import Routing
import Servant
import qualified System.Environment as Env

main :: IO ()
main = do
  env      <- getCurrentEnvironment
  appName  <- T.pack <$> Env.getEnv "APP_NAME"

  putStrLn $ "\nLoading " ++ show env ++ " " ++ show appName ++ " configuration..."
  cfg <- App.getAppConfig appName env

  putStrLn "\nRunning database migrations..."
  M.runMigrations $ App.getDBConn cfg

  putStrLn $ "\nWeb server running on port " <> show (App.getPort cfg) <> "..."
  run (App.getPort cfg) (app cfg)

app :: AppConfig -> Wai.Application
app cfg = logEntriesLogger (App.getLogEntriesConfig cfg)
  $ stdOutLogger (App.getEnv cfg)
  $ cors (const $ Just corsPolicy)
  $ serve api (readerServer cfg)

readerServer :: AppConfig -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: AppConfig -> AppT :~> Handler
readerToEither cfg = Nat $ \appT -> App.runAppT cfg appT

stdOutLogger :: Environment -> Middleware
stdOutLogger Test        = logStdoutDev
stdOutLogger Development = logStdoutDev
stdOutLogger Production  = logStdout

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let allowedMethods = simpleMethods <> ["DELETE", "PUT", "PATCH", "OPTIONS"]
      allowedHeaders = ["Content-Type"]
  in
    simpleCorsResourcePolicy { corsMethods = allowedMethods
                             , corsRequestHeaders = allowedHeaders
                             }
