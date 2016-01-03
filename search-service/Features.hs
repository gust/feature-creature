module Features
( indexFeatures
) where

import           CommonCreatures (WithErr)
import           Control.Monad.Except (runExceptT, throwError, liftIO)
import qualified Data.Text as Text
import           Features.Feature (findFeatureFiles)
import qualified Indexer
import           Products.CodeRepository (CodeRepository(..))

indexFeatures :: CodeRepository -> WithErr ()
indexFeatures (CodeRepository repositoryPath) = do
  let featureFileBasePath = Text.unpack repositoryPath
  featureFiles <- liftIO $ runExceptT $ findFeatureFiles featureFileBasePath
  case featureFiles of
    Left errorStr ->
      throwError errorStr
    Right features -> do
      (liftIO $ Indexer.indexFeatures $ map (featureFileBasePath ++) features) >> return ()
