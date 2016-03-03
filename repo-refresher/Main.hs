module Main where

import App
import AppConfig as Cfg
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import qualified Database.Persist.Postgresql as DB
import qualified Products.CodeRepository as Repo
import Products.Product as P

import qualified Indexer
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
    >>= (\results -> updateSearchIndex results prodID)
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

-- FIXME:
-- We don't need App here, this is a pure function.
-- App exists to keep the chain in `refreshRepo` moving along, but should definitely be removed.
parseStatusDiff :: Either String String
                -> App (Either String [Either ParseError Repo.FileModification])
parseStatusDiff (Left err)   = return $ Left err
parseStatusDiff (Right diff) = return $ Right (Repo.parseStatusDiff (lines diff))

updateSearchIndex :: (Either String [Either ParseError Repo.FileModification])
                  -> ProductID
                  -> App (Either String ())
updateSearchIndex (Left err) _            = return $ Left err
updateSearchIndex (Right fileMods) prodID =
  (mapM_ ((flip updateSearchIndex') prodID) fileMods) >> return (Right ())

updateSearchIndex' :: Either ParseError Repo.FileModification
                   -> ProductID
                   -> App (Either String ())
updateSearchIndex' (Left err) _           = return $ Left $ show err
updateSearchIndex' (Right fileMod) prodID =
  (updateSearchIndex'' fileMod prodID) >>= (\result -> return $ Right result)

updateSearchIndex'' :: Repo.FileModification -> ProductID -> App ()
updateSearchIndex'' fileMod pID = do
  gCfg <- reader getGitConfig
  esCfg <- reader getElasticSearchConfig
  case fileMod of
    (Repo.Added path)       -> liftIO $ Indexer.indexFeatures [path] pID gCfg esCfg
    (Repo.Modified path)    -> liftIO $ Indexer.indexFeatures [path] pID gCfg esCfg
    (Repo.Deleted path)     -> liftIO $ Indexer.deleteFeatures [path] esCfg
    (Repo.Copied _)      -> undefined
    (Repo.Renamed _)     -> undefined
    (Repo.TypeChanged _) -> undefined
    (Repo.Unmerged _)    -> undefined
    (Repo.Unknown _)     -> undefined
    (Repo.Broken _)      -> undefined
    (Repo.Unrecognized _ _) -> undefined
