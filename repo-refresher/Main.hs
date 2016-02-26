module Main where

import App
import AppConfig as Cfg
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import qualified Database.Persist.Postgresql as DB
import qualified Products.CodeRepository as Repo
import Products.Product as P

main :: IO ()
main = do
  appConfig <- Cfg.readConfig
  runReaderT refreshRepos appConfig

refreshRepos :: App ()
refreshRepos =
  getProductIDs
    >>= (\productIDs -> mapM_ refreshRepo productIDs)

getProductIDs :: App [ProductID]
getProductIDs =
  getProductEntities
    >>= (\entities -> return $ map P.toProductID entities)

getProductEntities :: App [DB.Entity Product]
getProductEntities =
  reader getDBConfig
    >>= (\cfg -> liftIO $ P.findProducts cfg)

refreshRepo :: ProductID -> App ()
refreshRepo prodID =
  reader getGitConfig
    >>= (\cfg -> (liftIO $ fetchRepoChanges prodID cfg) >> (liftIO $ getRepoDiff prodID cfg))

fetchRepoChanges :: ProductID -> GitConfig -> IO ()
fetchRepoChanges prodID gitConfig = do
  result <- runExceptT $ Repo.fetchRepo prodID gitConfig
  case result of
    Left err -> putStrLn err
    Right _  -> putStrLn $ "Successfully fetched prodID: " ++ (show prodID)

getRepoDiff :: ProductID -> GitConfig -> IO ()
getRepoDiff prodID gitConfig = do
  result <- runExceptT $ Repo.getDiff prodID gitConfig
  case result of
    Left err   -> putStrLn err
    Right diff -> putStrLn diff
