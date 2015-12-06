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
  featureFiles       <- runExceptT $ F.findFeatureFiles "/Users/toddmohney/workspace/feature-creature/.app-data/products/39/repo"

  let config = Config esUrl
  case featureFiles of
    Left errorStr ->
      putStrLn errorStr
    Right features -> do
      searchableFeatures <- sequence $ buildSearchableFeatures features
      replies <- sequence $ map indexFeature searchableFeatures
      putStrLn $ foldr (\x acc -> acc ++ "\n" ++ (show x)) "" replies

indexFeatures :: FilePath -> IO ()
indexFeatures rootPath = undefined
  {- featureFiles <- findAllFeatureFiles rootPath -}

buildSearchableFeatures :: [FilePath] -> [IO SearchableFeature]
buildSearchableFeatures filePaths =
  let fileDetails = map getFileConetnts filePaths
  in
    map (fmap buildSearchableFeature) fileDetails

getFileConetnts :: FilePath -> IO (FilePath, String)
getFileConetnts filePath = do
  let fullFilePath = "/Users/toddmohney/workspace/feature-creature/.app-data/products/39/repo" ++ filePath
  fileContents <- readFile fullFilePath
  return (filePath, fileContents)

buildSearchableFeature :: (FilePath, String) -> SearchableFeature
buildSearchableFeature (filePath, fileContents) =
  SearchableFeature { featurePath = pack filePath
                    , featureText = pack fileContents
                    }
