{-# LANGUAGE OverloadedStrings #-}

module Features.SearchableFeatureSpec where

import CommonCreatures
import Config.Config (ElasticSearchConfig(..))
import Control.Monad (liftM)
import Control.Monad.Except (runExceptT)
import Data.List (elem)
import Features.SearchableFeature as SF
import Test.Hspec

main :: IO ()
main = hspec spec

testFeature = SearchableFeature "some/path" "Hot Doggin" 1
esConfig    = ElasticSearchConfig "http://localhost:9200" "feature-creature-test" 1 0

cleanTestFeatures :: IO ()
cleanTestFeatures =
  (runExceptT $ deleteFeatures [(getFeaturePath testFeature)] esConfig)
    >> refreshFeaturesIndex esConfig

spec :: Spec
spec = before_ cleanTestFeatures $ do
  describe "Features.SearchableFeature" $ do
    describe "adding/searching/deleting a document" $ do
      it "provides create, read, delete operations" $ do
        baseline <- runExceptT $ searchFeatures 1 "Hot Doggin" esConfig
        liftM length baseline `shouldBe` (Right 0)

        results <- (runExceptT $ indexFeatures [testFeature] esConfig)
          >> refreshFeaturesIndex esConfig
          >> (runExceptT $ searchFeatures 1 "Hot Doggin" esConfig)

        liftM length results `shouldBe` (Right 1)
        liftM (elem testFeature) results `shouldBe` (Right True)

        postDeleteResults <- (runExceptT $ deleteFeatures [(getFeaturePath testFeature)] esConfig)
          >> refreshFeaturesIndex esConfig
          >> (runExceptT $ searchFeatures 1 "Hot Doggin" esConfig)

        liftM length postDeleteResults `shouldBe` (Right 0)

    describe "updating a document" $ do
      it "provides update operations" $ do
        baseline <- runExceptT $ searchFeatures 1 "Hot Doggin" esConfig
        liftM length baseline `shouldBe` (Right 0)

        results <- (runExceptT $ indexFeatures [testFeature] esConfig)
          >> refreshFeaturesIndex esConfig
          >> (runExceptT $ searchFeatures 1 "Hot Doggin" esConfig)

        liftM length results `shouldBe` (Right 1)
        liftM (elem testFeature) results `shouldBe` (Right True)

        let updatedFeature = (SearchableFeature "some/path" "Top Burger" 1)
        (runExceptT $ indexFeatures [updatedFeature] esConfig)
          >> refreshFeaturesIndex esConfig

        updatedResults <- (runExceptT $ searchFeatures 1 "Top Burger" esConfig)
        liftM (elem updatedFeature) updatedResults `shouldBe` (Right True)

        oldResults <- (runExceptT $ searchFeatures 1 "Hot Doggin" esConfig)
        liftM (elem updatedFeature) oldResults `shouldBe` (Right False)

