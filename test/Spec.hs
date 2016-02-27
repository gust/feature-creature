import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Hspec

import qualified Git
import Data.Either (isLeft)

main :: IO ()
main = hspec $ do
  describe "Git" $ do
    describe "parseStatusDiff" $ do
      context "when the diff contains a newly added file" $ do
        it "returns the Added type" $ do
          Git.parseStatusDiff ["A     some/file/path.hs"]
            `shouldBe` [Right $ Git.Added "some/file/path.hs"]

      context "when the diff contains a copied file" $ do
        it "returns the Copied type" $ do
          Git.parseStatusDiff ["C     some/file/path.hs"]
            `shouldBe` [Right $ Git.Copied "some/file/path.hs"]

      context "when the diff contains a deleted file" $ do
        it "returns the Deleted type" $ do
          Git.parseStatusDiff ["D     some/file/path.hs"]
            `shouldBe` [Right $ Git.Deleted "some/file/path.hs"]

      context "when the diff contains a modified file" $ do
        it "returns the Modified type" $ do
          Git.parseStatusDiff ["M     some/file/path.hs"]
            `shouldBe` [Right $ Git.Modified "some/file/path.hs"]

      context "when the diff contains a renamed file" $ do
        it "returns the Renamed type" $ do
          Git.parseStatusDiff ["R     some/file/path.hs"]
            `shouldBe` [Right $ Git.Renamed "some/file/path.hs"]

      context "when the diff contains a file who's type has changed" $ do
        it "returns the TypeChanged type" $ do
          Git.parseStatusDiff ["T     some/file/path.hs"]
            `shouldBe` [Right $ Git.TypeChanged "some/file/path.hs"]

      context "when the diff contains an unmerged file" $ do
        it "returns the Unmerged type" $ do
          Git.parseStatusDiff ["U     some/file/path.hs"]
            `shouldBe` [Right $ Git.Unmerged "some/file/path.hs"]

      context "when the diff contains an unknown file" $ do
        it "returns the Unknown type" $ do
          Git.parseStatusDiff ["X     some/file/path.hs"]
            `shouldBe` [Right $ Git.Unknown "some/file/path.hs"]

      context "when the diff contains an broken file" $ do
        it "returns the Broken type" $ do
          Git.parseStatusDiff ["B     some/file/path.hs"]
            `shouldBe` [Right $ Git.Broken "some/file/path.hs"]

      context "when the diff contains an unrecognized modification type" $ do
        it "returns a Left" $ do
          (isLeft . head $ Git.parseStatusDiff ["Z     some/file/path.hs"])
            `shouldBe` True

      context "when the diff cannot be parsed" $ do
        it "returns a Left" $ do
          (isLeft . head $ Git.parseStatusDiff ["alskdjfoaioen"])
            `shouldBe` True

