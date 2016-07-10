module Users.ApiSpec where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (isJust)
import Users
import qualified Data.Text as T
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hasValidationErrors" $ do
    it "returns True when the auth ID is blank" $
      hasValidationErrors (UserForm Nothing "" "Jeff" "Goldblum") `shouldBe` True

    it "returns True when the first name is blank" $
      hasValidationErrors (UserForm Nothing "abc123" "" "Goldblum") `shouldBe` True

    it "returns True when the last name is blank" $
      hasValidationErrors (UserForm Nothing "abc123" "Jeff" "") `shouldBe` True

    it "returns False when the first and last name are provided (without ID)" $
      hasValidationErrors (UserForm Nothing "abc123" "Jeff" "Goldblum") `shouldBe` False

    it "returns False when the first and last name are provided (with ID)" $
      hasValidationErrors (UserForm (Just 4) "abc123" "Jeff" "Goldblum") `shouldBe` False

  describe "formValidationErrors" $ do
    it "returns an error message when the model is invalid" $
      formValidationErrors invalidUserFormModel `shouldBe` "First and last name required."

    it "returns an empty message when the model is valid" $
      formValidationErrors validUserFormModel `shouldBe` T.empty

  describe "JSON marshalling" $ do
    it "defines To/FromJSON instances for User" $ do
      let encodedJson = prettyEncode user
      let decodedJson = AE.decode encodedJson :: Maybe User
      isJust decodedJson `shouldBe` True

    it "defines To/FromJSON instances for UserForm" $ do
      let encodedJson = prettyEncode validUserFormModel
      let decodedJson = AE.decode encodedJson :: Maybe UserForm
      isJust decodedJson `shouldBe` True


validUserFormModel :: UserForm
validUserFormModel = UserForm (Just 4) "abc123" "Jeff" "Goldblum"

invalidUserFormModel :: UserForm
invalidUserFormModel = UserForm (Just 4) "" "" ""

user :: User
user = User 1 "abc123" "bill" "murray"

prettyEncode :: AE.ToJSON a => a -> BS.ByteString
prettyEncode = AE.encodePretty' prettyConfig

prettyConfig :: AE.Config
prettyConfig = AE.Config { AE.confIndent = 2, AE.confCompare = mempty }
