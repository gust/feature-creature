module Main where

import Control.Monad.Except (runExceptT)
import Data.Text (pack)
import Data.Traversable (sequence)
import Features.Feature as F
import qualified Features.SearchableFeature as SF
import System.Environment (getEnv)

data AppConfig =
  AppConfig { elasticSearchUrl :: String
            , featureFilePath  :: String
            }

newtype CReader a = CReader { runConfigReader :: AppConfig -> a }

main :: IO ()
main = do
  esUrl         <- getEnv "FC_ELASTIC_SEARCH_URL"
  dataFilesPath <- getEnv "FC_DATA_FILES_PATH"
  let appConfig = (AppConfig esUrl (dataFilesPath ++ "/products/39/repo"))
  runConfigReader indexFeaturesCR appConfig

indexFeaturesCR :: CReader (IO ())
indexFeaturesCR = CReader $ (\c -> indexFeatures c)

indexFeatures :: AppConfig -> IO ()
indexFeatures appConfig = do
  featureFiles <- runExceptT $ F.findFeatureFiles (featureFilePath appConfig)
  case featureFiles of
    Left errorStr ->
      putStrLn errorStr

    Right features -> do
      searchableFeatures <- sequence $ buildSearchableFeatures features appConfig
      replies            <- SF.indexFeatures searchableFeatures
      putStrLn $ foldr (\x acc -> acc ++ "\n" ++ (show x)) "" replies

buildSearchableFeatures :: [FilePath] -> AppConfig -> [IO SF.SearchableFeature]
buildSearchableFeatures filePaths appConfig =
  let fileDetails = map ((flip getFileConetnts) appConfig) filePaths
  in
    map (fmap buildSearchableFeature) fileDetails

getFileConetnts :: FilePath -> AppConfig -> IO (FilePath, String)
getFileConetnts filePath appConfig = do
  let fullFilePath = (featureFilePath appConfig) ++ filePath
  fileContents <- readFile fullFilePath
  return (filePath, fileContents)

buildSearchableFeature :: (FilePath, String) -> SF.SearchableFeature
buildSearchableFeature (filePath, fileContents) =
  SF.SearchableFeature { SF.featurePath = pack filePath
                       , SF.featureText = pack fileContents
                       }
