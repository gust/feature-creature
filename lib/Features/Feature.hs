{-# LANGUAGE OverloadedStrings #-}

module Features.Feature where
  import CommonCreatures (WithErr)
  import Control.Applicative ((<$>))
  import Data.List (stripPrefix, partition)
  import Data.Maybe (mapMaybe)
  import Data.Text (Text, pack, unpack, splitOn)
  import Data.Tree (Tree(Node))
  import Control.Monad.Except (throwError)
  import Control.Monad.IO.Class (liftIO)
  import qualified Data.Tree as T
  import System.Exit (ExitCode(ExitFailure, ExitSuccess))
  import System.Process (readProcessWithExitCode)

  type Feature                = String
  type FeatureFile            = FilePath
  type FeatureTree            = T.Tree Feature

  getFeatures :: FilePath -> WithErr FeatureTree
  getFeatures path = buildFeatureTree <$> findFeatureFiles path

  buildFeatureTree :: [FeatureFile] -> FeatureTree
  buildFeatureTree = foldr (\x acc -> addToFileTree acc (splitFilename x)) rootNode
    where
      splitFilename :: FeatureFile -> [Text]
      splitFilename fn = splitOn "/" (pack fn)

      rootNode :: Tree String
      rootNode = Node "featuresRoot" []

  addToFileTree :: FeatureTree -> [Text] -> FeatureTree
  addToFileTree featureTree []                       = featureTree
  addToFileTree (Node label forest) [file]           = Node label $ forest ++ [(createNode $ unpack file)]
  addToFileTree (Node label forest) (directory:rest) =
    let (matches, nonMatches) = partition (matchesLabel directory) forest in
        case matches of
          [] -> do
            let newNode = createNode $ unpack directory
            Node label $ (addToFileTree newNode rest):forest
          [node] -> do
            Node label $ (addToFileTree node rest):nonMatches -- effectively replace the matching node
          (_:_) -> error "There should be a maximum of 1 match in the forest"

  matchesLabel :: Text -> FeatureTree -> Bool
  matchesLabel file (Node label _) = (unpack file) == label

  createNode :: Feature -> FeatureTree
  createNode file = Node file []

  findFeatureFiles :: FilePath -> WithErr [FeatureFile]
  findFeatureFiles path = do
    result <- liftIO $ readProcessWithExitCode "find" [path, "-type", "f", "-name", "*.feature"] ""
    parseFeatureFiles result path

  parseFeatureFiles :: (ExitCode, String, String) -> FilePath -> WithErr [FeatureFile]
  parseFeatureFiles (ExitFailure _, stdout, stderr) _      = throwError $ stderr ++ stdout
  parseFeatureFiles (ExitSuccess, stdout, stderr) basePath = return $ mapMaybe stripPath $ outputLines
    where
      stripPath = (stripPrefix basePath)
      outputLines = lines (stderr ++ stdout)
