{-# LANGUAGE OverloadedStrings #-}

module Features.SearchableFeatureSpec where

import Config.Config (ElasticSearchConfig(..))
import Data.List (elem)
import Features.SearchableFeature as SF
import Test.Hspec

main :: IO ()
main = hspec spec

testFeature = SearchableFeature "some/path" "Hot Doggin" 1
esConfig    = ElasticSearchConfig "http://localhost:9200" "feature-creature-test"

cleanTestFeatures :: IO ()
cleanTestFeatures =
  deleteFeatures [(getFeaturePath testFeature)] esConfig
    >> refreshFeaturesIndex esConfig

spec :: Spec
spec = before_ cleanTestFeatures $ do
  describe "Features.SearchableFeature" $ do
    describe "adding/searching/deleting a document" $ do
      it "provides create, read, delete operations" $ do
        baseline <- searchFeatures 1 "Hot Doggin" esConfig
        length baseline `shouldBe` 0

        results <- indexFeatures [testFeature] esConfig
          >> refreshFeaturesIndex esConfig
          >> searchFeatures 1 "Hot Doggin" esConfig

        length results `shouldBe` 1
        elem testFeature results `shouldBe` True

        postDeleteResults <- deleteFeatures [(getFeaturePath testFeature)] esConfig
          >> refreshFeaturesIndex esConfig
          >> searchFeatures 1 "Hot Doggin" esConfig
        length postDeleteResults `shouldBe` 0

    describe "updating a document" $ do
      it "provides update operations" $ do
        baseline <- searchFeatures 1 "Hot Doggin" esConfig
        length baseline `shouldBe` 0

        results <- indexFeatures [testFeature] esConfig
          >> refreshFeaturesIndex esConfig
          >> searchFeatures 1 "Hot Doggin" esConfig

        length results `shouldBe` 1
        elem testFeature results `shouldBe` True

        let updatedFeature = (SearchableFeature "some/path" "Top Burger" 1)
        indexFeatures [updatedFeature] esConfig
          >> refreshFeaturesIndex esConfig

        updatedResults <- searchFeatures 1 "Top Burger" esConfig
        elem updatedFeature updatedResults `shouldBe` True

        oldResults <- searchFeatures 1 "Hot Doggin" esConfig
        elem updatedFeature oldResults `shouldBe` False

