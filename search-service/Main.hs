module Main where

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

main :: IO ()
main = do
  appConfig <- readConfig
  runReaderT indexFeatures appConfig

readConfig :: IO AppConfig
readConfig = do
  esUrl         <- getEnv "FC_ELASTIC_SEARCH_URL"
  dataFilesPath <- getEnv "FC_DATA_FILES_PATH"
  let baseFilePath = dataFilesPath ++ "/products/39/repo"
  return $ AppConfig esUrl baseFilePath

indexFeatures :: ReaderT AppConfig IO ()
indexFeatures = do
  basePath     <- reader featureFilePath
  featureFiles <- liftIO $ runExceptT $ F.findFeatureFiles basePath
  case featureFiles of
    Left errorStr ->
      liftIO $ putStrLn errorStr

    Right features -> do
      searchableFeatures <- buildSearchableFeatures features
      replies            <- lift $ SF.indexFeatures searchableFeatures
      lift $ putStrLn $ foldr (\x acc -> acc ++ "\n" ++ (show x)) "" replies

buildSearchableFeatures :: [FilePath] -> ReaderT AppConfig IO [SF.SearchableFeature]
buildSearchableFeatures filePaths = do
  basePath <- reader featureFilePath
  let fileDetails = map ((flip getFileConetnts) basePath) filePaths
  lift $ sequence $ map (fmap buildSearchableFeature) fileDetails

getFileConetnts :: FilePath -> FilePath -> IO (FilePath, String)
getFileConetnts filePath basePath = do
  let fullFilePath = basePath ++ filePath
  fileContents <- readFile fullFilePath
  return (filePath, fileContents)

buildSearchableFeature :: (FilePath, String) -> SF.SearchableFeature
buildSearchableFeature (filePath, fileContents) =
  SF.SearchableFeature { SF.featurePath = pack filePath
                       , SF.featureText = pack fileContents
                       }
