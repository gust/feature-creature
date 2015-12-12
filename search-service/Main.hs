module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Data.Text (pack)
import Features.Feature as F
import qualified Features.SearchableFeature as SF
import System.Environment (getEnv)

data AppConfig =
  AppConfig { elasticSearchUrl :: String
            , featureFilePath  :: String
            }

type App a = ReaderT AppConfig IO a

main :: IO ()
main = do
  appConfig <- readConfig
  runReaderT indexFeatures appConfig

readConfig :: IO AppConfig
readConfig = do
  esUrl         <- getEnv "FC_ELASTIC_SEARCH_URL"
  dataFilesPath <- getEnv "FC_DATA_FILES_PATH"
  return $ AppConfig esUrl dataFilesPath

indexFeatures :: App ()
indexFeatures = do
  forever $ do
    basePath <- reader featureFilePath
    repoPath <- liftIO promptForFeaturePath

    let featureFileBasePath = basePath ++ repoPath
    -- this could become useful as log output
    liftIO $ putStrLn $ "Finding feature files at: " ++ featureFileBasePath
    featureFiles <- liftIO $ runExceptT $ F.findFeatureFiles featureFileBasePath
    case featureFiles of
      Left errorStr ->
        liftIO $ putStrLn errorStr
      Right features ->
        liftIO $ indexFeatures' $ map (\featurePath -> featureFileBasePath ++ featurePath) features
    lift $ liftIO $ threadDelay 1000

indexFeatures' :: [FilePath] -> IO ()
indexFeatures' features = do
  searchableFeatures <- buildSearchableFeatures features
  replies            <- SF.indexFeatures searchableFeatures
  -- this could become useful as log output
  putStrLn $ foldr (\x acc -> acc ++ "\n" ++ (show x)) "" replies

buildSearchableFeatures :: [FilePath] -> IO [SF.SearchableFeature]
buildSearchableFeatures filePaths = do
  -- this could be parallelized
  sequence $ map ((fmap buildSearchableFeature) . getFileContents) filePaths

getFileContents :: FilePath -> IO (FilePath, String)
getFileContents filePath = do
  fileContents <- readFile filePath
  return (filePath, fileContents)

promptForFeaturePath :: IO String
promptForFeaturePath = do
  liftIO $ putStrLn "Enter features path: " >> getLine

buildSearchableFeature :: (FilePath, String) -> SF.SearchableFeature
buildSearchableFeature (filePath, fileContents) =
  SF.SearchableFeature { SF.featurePath = pack filePath
                       , SF.featureText = pack fileContents
                       }
