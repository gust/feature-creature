module Data.DirectoryTreeSpec where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Tree
import Data.DirectoryTree
import Data.Either (isRight)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JSON (de)serialization" $
    it "can be encoded and decoded from JSON" $ do
      let encodedJson = BS.unpack . prettyEncode $ testTree
      let decodedJson = AE.eitherDecode (BS.pack encodedJson) :: Either String DirectoryTree
      {- putStrLn encodedJson -}
      {- case decodedJson of -}
        {- (Left err) -> putStrLn err -}
        {- (Right val) -> putStr $ show decodedJson -}
      isRight decodedJson `shouldBe` True

testTree :: DirectoryTree
testTree = DirectoryTree rootNode

rootNode   = Node (FileDescription "features" "features") [creatures]
creatures  = Node (FileDescription "creatures" "features/creatures") [swampThing, wolfman]
swampThing = Node (FileDescription "swamp-thing" "features/creatures/swamp-thing") [
  Node (FileDescription "vegetable-mind-control.feature" "features/creatures/swamp-thing/vegetable-mind-control.feature") [],
  Node (FileDescription "limb-regeneration.feature" "features/creatures/swamp-thing/limb-regeneration.feature") []
  ]
wolfman = Node (FileDescription "wolfman" "features/creatures/wolfman") [
  Node (FileDescription "shape-shifting.feature" "features/creatures/wolfman/shape-shifting.feature") [],
  Node (FileDescription "animal-instincts.feature" "features/creatures/wolfman/animal-instincts.feature") []
  ]

prettyEncode :: AE.ToJSON a => a -> BS.ByteString
prettyEncode = AE.encodePretty' prettyConfig

prettyConfig :: AE.Config
prettyConfig = AE.Config { AE.confIndent = 2, AE.confCompare = mempty }
