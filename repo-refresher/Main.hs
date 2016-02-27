module Main where

import App
import AppConfig as Cfg
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import qualified Database.Persist.Postgresql as DB
import qualified Products.CodeRepository as Repo
import Products.Product as P

import Text.ParserCombinators.Parsec (ParseError)

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
  fetchRepoChanges prodID
    >> getRepoDiff prodID
    >>= (\diff -> parseStatusDiff diff)
    >> return ()

fetchRepoChanges :: ProductID -> App ()
fetchRepoChanges prodID = do
  gitConfig <- reader getGitConfig
  result    <- liftIO $ runExceptT $ Repo.fetchRepo prodID gitConfig
  case result of
    Left err -> liftIO $ putStrLn err
    Right _  -> liftIO $ putStrLn $ "Successfully fetched prodID: " ++ (show prodID)

getRepoDiff :: ProductID -> App (Either String String)
getRepoDiff prodID = do
  gitConfig <- reader getGitConfig
  liftIO $ runExceptT $ Repo.getStatusDiff prodID gitConfig

-- we don't need App here, this is a pure function
parseStatusDiff :: Either String String -> App (Either String [Either ParseError Repo.FileModification])
parseStatusDiff (Left err)   = return $ Left err
parseStatusDiff (Right diff) = return $ Right (Repo.parseStatusDiff (lines diff))
