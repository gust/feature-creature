module Features
( indexFeatures
) where

import           Control.Monad.Except (runExceptT)
import qualified Data.Text as Text
import           Features.Feature (findFeatureFiles)
import qualified Indexer
import           Products.CodeRepository (CodeRepository(..))

indexFeatures :: CodeRepository -> IO ()
indexFeatures (CodeRepository repositoryPath) = do
  let featureFileBasePath = Text.unpack repositoryPath
  putStrLn $ "Finding feature files at: " ++ (Text.unpack repositoryPath)

  featureFiles <- runExceptT $ findFeatureFiles featureFileBasePath
  case featureFiles of
    Left errorStr ->
      putStrLn errorStr
    Right features ->
      Indexer.indexFeatures $ map (\featurePath -> featureFileBasePath ++ featurePath) features
