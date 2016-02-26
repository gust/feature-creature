module Main where

import App
import AppConfig as Cfg
import Control.Monad.Reader
import qualified Database.Persist.Postgresql as DB
import Products.Product as P

main :: IO ()
main = do
  appConfig <- Cfg.readConfig
  runReaderT refreshRepos appConfig

refreshRepos :: App ()
refreshRepos =
  getProducts
    >>= (\products -> liftIO $ printProducts products)

getProducts :: App [Product]
getProducts = do
  getProductEntities
    >>= (\entities -> return $ map P.toProduct entities)

getProductEntities :: App [DB.Entity Product]
getProductEntities =
  reader getDBConfig
    >>= (\cfg -> liftIO $ P.findProducts cfg)

printProducts :: [Product] -> IO ()
printProducts = mapM_ (putStrLn . show . productName)
