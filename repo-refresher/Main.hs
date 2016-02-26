module Main where

import App
import AppConfig as Cfg
import Control.Monad.Reader
import Products.Product as P

main :: IO ()
main = do
  appConfig <- Cfg.readConfig
  runReaderT refreshRepos appConfig

refreshRepos :: App ()
refreshRepos = do
  dbCfg    <- reader getDBConfig
  products <- liftIO $ P.findProducts dbCfg
  liftIO $ mapM_ (putStrLn . show . productName . toProduct) products
