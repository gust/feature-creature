module Home.ApiSpec where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (isJust)
import Users.Api
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JSON marshalling" $ do
    it "defines To/FromJSON instances for User" $ do
      let encodedJson = prettyEncode user
      let decodedJson = AE.decode encodedJson :: Maybe User
      isJust decodedJson `shouldBe` True


user :: User
user = User 1 "bill" "murray"

prettyEncode :: AE.ToJSON a => a -> BS.ByteString
prettyEncode = AE.encodePretty' prettyConfig

prettyConfig :: AE.Config
prettyConfig = AE.Config { AE.confIndent = 2, AE.confCompare = mempty }
