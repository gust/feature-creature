{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Products.FeaturesAPI
( FeaturesAPI
, FeatureAPI
, APIFeature (..)
, productsFeatures
, productsFeature
) where

import App
import AppConfig (ElasticSearchConfig, getGitConfig, getElasticSearchConfig)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Data.Aeson
import Data.DirectoryTree
import Data.Text (Text, pack, unpack)
import qualified Features.Feature as F
import qualified Features.SearchableFeature as SF
import qualified Products.CodeRepository as CR
import qualified Products.Product as P
import Servant

data APIFeature = APIFeature { featureID :: F.FeatureFile
                             , description :: F.Feature
                             } deriving (Show)

type FeaturesAPI = "features" :> QueryParam "search" String :> Get '[JSON] DirectoryTree
type FeatureAPI  = "feature"  :> QueryParam "path" F.FeatureFile
                              :> Get '[JSON] APIFeature

instance FromText F.FeatureFile where
  fromText path = Just $ F.FeatureFile (unpack path)

instance ToJSON APIFeature where
  toJSON (APIFeature (F.FeatureFile featID) (F.Feature desc)) =
    object [ "featureID"   .= featID
           , "description" .= desc
           ]
productsFeatures :: P.ProductID -> Maybe String -> App DirectoryTree
productsFeatures prodID (Just searchTerm) = do
  esConfig <- reader getElasticSearchConfig
  liftIO $ searchFeatures prodID (pack searchTerm) esConfig
productsFeatures prodID Nothing = do
  featuresPath <- CR.codeRepositoryDir prodID <$> reader getGitConfig
  result       <- liftIO $ runExceptT (F.getFeatures featuresPath)
  case result of
    Left msg   -> error msg
    Right tree -> return tree

searchFeatures :: P.ProductID -> Text -> ElasticSearchConfig -> IO DirectoryTree
searchFeatures prodID searchTerm esConfig =
  (liftIO $ runExceptT (SF.searchFeatures prodID searchTerm esConfig))
    >>= \fs -> case fs of
                 (Left err) -> error err
                 (Right features) -> return $ F.buildDirectoryTree . parseFeatureFiles $ features

parseFeatureFiles :: [SF.SearchableFeature] -> [F.FeatureFile]
parseFeatureFiles = map (F.FeatureFile . unpack . SF.getFeaturePath)

productsFeature :: P.ProductID -> Maybe F.FeatureFile -> App APIFeature
productsFeature _ Nothing = error "Missing required query param 'path'"
productsFeature prodID (Just (F.FeatureFile path)) = do
  featuresPath <- CR.codeRepositoryDir prodID <$> reader getGitConfig
  result       <- liftIO $ runExceptT (F.getFeature $ F.FeatureFile (featuresPath ++ path))
  case result of
    Left msg      -> error msg
    Right feature ->
      return $ APIFeature { featureID = F.FeatureFile path
                          , description = feature
                          }

