module Main where

import Config.AppConfig as App
import Config.Environment (getCurrentEnvironment)
import Data.Text as T
import qualified System.Environment as Env

main :: IO ()
main = do
  env      <- getCurrentEnvironment
  appName  <- T.pack <$> Env.getEnv "APP_NAME"

  putStrLn $ "\nLoading " ++ show env ++ " " ++ show appName ++ " configuration..."
  cfg <- App.getAppConfig appName env

  putStrLn "hi"
