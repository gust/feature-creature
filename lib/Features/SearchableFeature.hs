{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Features.SearchableFeature
( SearchableFeature (..)
, indexFeatures
, searchFeatures
) where

import Config.Config (ElasticSearchConfig(..))
import Database.Bloodhound
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
                    } deriving (Show, Generic)

instance ToJSON   SearchableFeature
instance FromJSON SearchableFeature

-- handle failure
indexFeatures :: [SearchableFeature] -> ElasticSearchConfig -> IO ()
indexFeatures searchableFeatures esConfig =
  let indicies = map (createBulkIndex (getIndexName esConfig)) searchableFeatures 
      stream = V.fromList indicies :: V.Vector BulkOperation
  in
    -- we're outputting something unhelpful here to hide the
    -- internal BH types from the user. we get a Response back
    -- which we can log in the future
    withBH' esConfig (bulk stream)
      >>= putStrLn . ("ElasticSearch Reply: " ++) . show
  where
    createBulkIndex indexName f =
      BulkIndex
        (IndexName (pack indexName))
        (MappingName "feature")
        (DocId (getFeaturePath f))
        (toJSON f)

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

withBH' :: ElasticSearchConfig -> BH IO a -> IO a
withBH' esConfig a = withBH defaultManagerSettings (Server (pack (getESUrl esConfig))) a
