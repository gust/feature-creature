{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Features.SearchableFeature where
  import Database.Bloodhound
  import Database.Bloodhound.Types as BHTypes
  import Data.Aeson
  import Data.Text (Text)
  import qualified Data.Vector as V
  import GHC.Generics (Generic)
  import Network.HTTP.Client

  data SearchableFeature =
    SearchableFeature { featurePath :: Text
                      , featureText :: Text
                      } deriving (Show, Generic)

  instance ToJSON   SearchableFeature
  instance FromJSON SearchableFeature

  sampleSearchableFeature :: SearchableFeature
  sampleSearchableFeature =
    SearchableFeature { featurePath = "/some/cool/path"
                      , featureText = "Given blah When blah Then more blah"
                      }

  indexFeatures :: [SearchableFeature] -> IO BHTypes.Reply
  indexFeatures searchableFeatures =
    let indicies = map createBulkIndex searchableFeatures
        stream = V.fromList indicies :: V.Vector BulkOperation
    in
      withBH' $ bulk stream
    where
      createBulkIndex f =
        BulkIndex
          testIndex
          testMapping
          (DocId (featurePath f))
          (toJSON f)

  indexFeature :: SearchableFeature -> IO BHTypes.Reply
  indexFeature searchableFeature =
    withBH' $ indexDocument testIndex testMapping defaultIndexDocumentSettings searchableFeature (DocId (featurePath searchableFeature))

  searchFeatures :: Text -> IO BHTypes.Reply
  searchFeatures queryStr =
    withBH' $ searchByIndex testIndex search
    where
      search = mkSearch (Just query) (Just filter)
      filter = IdentityFilter <&&> IdentityFilter
      {- filter = IdentityFilter <||> IdentityFilter -}
      {- query = TermQuery (Term "feature" queryStr) Nothing -}
      query = QueryMatchQuery $ mkMatchQuery (FieldName "featureText") (QueryString queryStr)

  withBH' = withBH defaultManagerSettings testServer

  testMapping :: MappingName
  testMapping = MappingName "feature"

  testIndex :: IndexName
  testIndex = IndexName "feature-creature"

  testServer :: Server
  testServer = Server "http://localhost:9200"
