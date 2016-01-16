{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Features.SearchableFeature
( SearchableFeature (..)
, indexFeatures
, searchFeatures
) where
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

indexFeatures :: [SearchableFeature] -> IO ()
indexFeatures searchableFeatures =
  let indicies = map createBulkIndex searchableFeatures
      stream = V.fromList indicies :: V.Vector BulkOperation
  in
    -- we're outputting something unhelpful here to hide the
    -- internal BH types from the user. we get a Response back
    -- which we can log in the future
    withBH' (bulk stream) >>= putStrLn . ("ElasticSearch Reply: " ++) . show
  where
    createBulkIndex f =
      BulkIndex
        featureCreatureIndex
        featureMapping
        (DocId (getFeaturePath f))
        (toJSON f)

-- search is not constrained to a mapping
searchFeatures :: ProductID -> Text -> IO [SearchableFeature]
searchFeatures prodID queryStr = do
  reply <- withBH' $ searchByIndex featureCreatureIndex search
  let results = eitherDecode (responseBody reply) :: Either String (SearchResult SearchableFeature)
  case fmap (hits . searchHits) results of
    Left str   -> return [ (SearchableFeature (pack str) (pack str) prodID) ]
    Right searchResultHits -> return $ mapMaybe hitSource searchResultHits
  where
    query = QueryMatchQuery $ mkMatchQuery (FieldName "getFeatureText") (QueryString queryStr)
    productFilter = BoolFilter (MustMatch (Term "getProductID" (pack $ show prodID)) False)
    searchFilter = IdentityFilter <&&> productFilter
    search = mkSearch (Just query) (Just searchFilter)

withBH' :: BH IO a -> IO a
withBH' = withBH defaultManagerSettings testServer

featureMapping :: MappingName
featureMapping = MappingName "feature"

featureCreatureIndex :: IndexName
featureCreatureIndex = IndexName "feature-creature"

-- we need to add this to Config
testServer :: Server
testServer = Server "http://localhost:9200"
