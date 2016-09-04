module Main where

import App
import AppConfig as Cfg
import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import qualified Database.Persist.Postgresql as DB
import Database.Types (runPool)
import Features.Feature (FeatureFile (..))
import qualified Git.Git as Git
import qualified Indexer
import Products.Product as P
import qualified Products.ProductRepo as PR
import Retry (withRetry)

main :: IO ()
main = Cfg.readConfig >>= \appConfig ->
  let interval = (Cfg.refreshInterval (getGitConfig appConfig))
  in runReaderT refreshRepos appConfig
      >> threadDelay (interval * 1000 * 1000)
      >> main

refreshRepos :: App ()
refreshRepos = getProductIDs >>= (mapM_ refreshRepo)

getProductIDs :: App [ProductID]
getProductIDs = (withRetry getProductEntities) >>= \entities ->
  return $ map P.toProductID entities

getProductEntities :: App [DB.Entity Product]
getProductEntities = (reader getDBConfig) >>= \cfg ->
  liftIO $ runReaderT (runPool P.findProducts) (getPool cfg)

refreshRepo :: ProductID -> App ()
refreshRepo prodID =
  fetchRepoChanges prodID
    >> getRepoDiff prodID
    >>= (\diff              -> parseStatusDiff diff)
    >>= (\diffResults       -> updateSearchIndex diffResults prodID)
    >>= (\indexResults      -> updateGitRepo indexResults prodID)
    >>= (\repoUpdateResults -> printResults repoUpdateResults prodID)

printResults :: Either String String -> ProductID -> App ()
printResults (Left err)  prodID = liftIO $ putStrLn ("Failed to update repo " ++ (show prodID) ++ "" ++ err)
printResults (Right msg) prodID = liftIO $ putStrLn ("Successfully updated repo " ++ (show prodID) ++ " " ++ msg)

fetchRepoChanges :: ProductID -> App ()
fetchRepoChanges prodID = do
  gitConfig <- reader getGitConfig
  result    <- liftIO $ runExceptT $ PR.fetchRepo prodID gitConfig
  case result of
    Left err -> liftIO $ putStrLn err
    Right _  -> liftIO $ putStrLn $ "Successfully fetched prodID: " ++ (show prodID)

getRepoDiff :: ProductID -> App (Either String String)
getRepoDiff prodID = (reader getGitConfig) >>= \gitCfg ->
  liftIO . runExceptT $ PR.getStatusDiff prodID gitCfg

-- FIXME:
-- We don't need App here, this is a pure function.
-- App exists to keep the chain in `refreshRepo` moving along, but should definitely be removed.
parseStatusDiff :: Either String String -> App (Either String [PR.ParseResult])
parseStatusDiff (Left err)   = return $ Left err
parseStatusDiff (Right diff) = return $ Right (PR.parseStatusDiff (lines diff))

updateSearchIndex :: (Either String [PR.ParseResult]) -> ProductID -> App (Either String ())
updateSearchIndex (Left err) _ = return $ Left err
updateSearchIndex (Right fileMods) prodID =
  (mapM ((flip updateSearchIndex') prodID) fileMods)
    >>= (\results -> liftIO $ mapM_ printSearchIndexResults results)
    >> return (Right ())

printSearchIndexResults :: Either String () -> IO ()
printSearchIndexResults (Left err) = putStrLn err
printSearchIndexResults (Right _ ) = putStrLn "Successful index!"

updateSearchIndex' :: PR.ParseResult -> ProductID -> App (Either String ())
updateSearchIndex' (Left err) _ = return $ Left $ show err
updateSearchIndex' (Right fileMod) prodID =
  (updateSearchIndex'' fileMod prodID) >>= (\result -> return $ Right result)

updateSearchIndex'' :: PR.FileModification -> ProductID -> App ()
updateSearchIndex'' fileMod pID = do
  gCfg <- reader getGitConfig
  esCfg <- reader getElasticSearchConfig
  case fileMod of
    (PR.Added path)    -> liftIO $ Indexer.indexFeatures [FeatureFile path] pID gCfg esCfg
    (PR.Modified path) -> liftIO $ Indexer.indexFeatures [FeatureFile path] pID gCfg esCfg
    (PR.Deleted path)  -> liftIO $ Indexer.deleteFeatures [FeatureFile path] esCfg
    _                    -> return ()
    {- (PR.Copied _)      -> undefined -}
    {- (PR.Renamed _)     -> undefined -}
    {- (PR.TypeChanged _) -> undefined -}
    {- (PR.Unmerged _)    -> undefined -}
    {- (PR.Unknown _)     -> undefined -}
    {- (PR.Broken _)      -> undefined -}
    {- (PR.Unrecognized _ _) -> undefined -}

updateGitRepo :: Either String () -> ProductID -> App (Either String String)
updateGitRepo (Left err) _     = return $ Left err
updateGitRepo (Right _) prodID = do
  reader getGitConfig
    >>= (\gitConfig -> liftIO $ runExceptT $ Git.pull $ PR.codeRepositoryDir prodID gitConfig)
    >>= (\result -> return result)

