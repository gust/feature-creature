{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Features.SearchableFeature
( SearchableFeature (..)
, createFeaturesIndex
, deleteFeaturesIndex
, indexFeatures
, deleteFeatures
, refreshFeaturesIndex
, searchFeatures
) where

import Config.Config (ElasticSearchConfig(..))
import Database.Bloodhound as BH
import Data.Aeson
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Client
import Products.Product (ProductID)

data SearchableFeature =
  SearchableFeature { getFeaturePath :: Text
                    , getFeatureText :: Text
                    , getProductID   :: ProductID
                    } deriving (Show, Eq, Generic)

instance ToJSON   SearchableFeature
instance FromJSON SearchableFeature

createFeaturesIndex :: ElasticSearchConfig -> IO ()
createFeaturesIndex esConfig =
  withBH' esConfig (createIndex defaultIndexSettings (indexName esConfig))
    >> putStrLn "ElasticSearch IndexCreated"

deleteFeaturesIndex :: ElasticSearchConfig -> IO ()
deleteFeaturesIndex esConfig =
  withBH' esConfig (deleteIndex (indexName esConfig))
    >> putStrLn "ElasticSearch IndexDeleted"

refreshFeaturesIndex :: ElasticSearchConfig -> IO ()
refreshFeaturesIndex esConfig =
  withBH' esConfig (refreshIndex (indexName esConfig))
    >> putStrLn "ElasticSearch RefreshIndex"

-- TODO: handle failure
indexFeatures :: [SearchableFeature] -> ElasticSearchConfig -> IO ()
indexFeatures searchableFeatures esConfig =
  let indicies = map (createBulkIndex (getIndexName esConfig)) searchableFeatures
      stream = V.fromList indicies :: V.Vector BulkOperation
  in
    -- we're outputting the response in lieu of a proper log
    withBH' esConfig (bulk stream)
      >>= putStrLn . ("ElasticSearch BulkCreate Reply: " ++) . show

-- TODO: better identify [Text] as the ID of the document
deleteFeatures :: [Text] -> ElasticSearchConfig -> IO ()
deleteFeatures docIDs esConfig =
  let indicies = map (createBulkDelete (getIndexName esConfig)) docIDs
      stream = V.fromList indicies :: V.Vector BulkOperation
  in
    -- we're outputting the response in lieu of a proper log
    withBH' esConfig (bulk stream)
      >>= putStrLn . ("ElasticSearch BulkDelete Reply: " ++) . show

searchFeatures :: ProductID -> Text -> ElasticSearchConfig -> IO [SearchableFeature]
searchFeatures prodID queryStr esConfig = do
  reply <- withBH' esConfig $ searchByIndex (IndexName (pack (getIndexName esConfig))) search
  let results = eitherDecode (responseBody reply) :: Either String (SearchResult SearchableFeature)
  case fmap (hits . searchHits) results of
    Left str   -> return [ (SearchableFeature (pack str) (pack str) prodID) ]
    Right searchResultHits -> return $ mapMaybe hitSource searchResultHits
  where
    query         = QueryMatchQuery $ mkMatchQuery (FieldName "getFeatureText") (QueryString queryStr)
    productFilter = BoolFilter (MustMatch (Term "getProductID" (pack $ show prodID)) False)
    searchFilter  = IdentityFilter <&&> productFilter
    search        = mkSearch (Just query) (Just searchFilter)

createBulkIndex :: String -> SearchableFeature -> BulkOperation
createBulkIndex idxName f =
  BulkIndex
    (IndexName (pack idxName))
    (MappingName "feature")
    (DocId (getFeaturePath f))
    (toJSON f)

createBulkDelete :: String -> Text -> BulkOperation
createBulkDelete idxName f =
  BulkDelete
    (IndexName (pack idxName))
    (MappingName "feature")
    (DocId f)

indexName :: ElasticSearchConfig -> IndexName
indexName esConfig = IndexName $ pack (getIndexName esConfig)

withBH' :: ElasticSearchConfig -> BH IO a -> IO a
withBH' esConfig a = withBH defaultManagerSettings (Server (pack (getESUrl esConfig))) a
