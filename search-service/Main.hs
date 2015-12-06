module Main where

import CommonCreatures (WithErr)
import Config
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.Traversable (sequence)
import Features.Feature as F
import Features.SearchableFeature as SF
import System.Environment (getEnv)

main :: IO ()
main = do
  esUrl              <- getEnv "FC_ELASTIC_SEARCH_URL"
  projectsBaseDir    <- getEnv "FC_SERVER_PROJECT_ROOT"
  featureFiles       <- runExceptT $ F.findFeatureFiles "/Users/toddmohney/workspace/feature-creature/.app-data/products/39/repo"

  let config = Config esUrl
  case featureFiles of
    Left errorStr ->
      putStrLn errorStr
    Right features -> do
      searchableFeatures <- sequence $ buildSearchableFeatures projectsBaseDir features
      replies <- SF.indexFeatures searchableFeatures
      putStrLn $ foldr (\x acc -> acc ++ "\n" ++ (show x)) "" replies

buildSearchableFeatures :: FilePath -> [FilePath] -> [IO SearchableFeature]
buildSearchableFeatures basePath filePaths =
  let fileDetails = map (getFileConetnts basePath) filePaths
  in
    map (fmap buildSearchableFeature) fileDetails

getFileConetnts :: FilePath -> FilePath -> IO (FilePath, String)
getFileConetnts basePath filePath = do
  let fullFilePath = basePath ++ "/.app-data/products/39/repo" ++ filePath
  fileContents <- readFile fullFilePath
  return (filePath, fileContents)

buildSearchableFeature :: (FilePath, String) -> SearchableFeature
buildSearchableFeature (filePath, fileContents) =
  SearchableFeature { featurePath = pack filePath
                    , featureText = pack fileContents
                    }
