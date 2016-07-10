module Users.RequestSpec
  ( main
  , spec
  ) where

import Helpers.TestWebServer

import App (AppConfig (..))
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Time.Clock as Clock
import qualified Models as M
import Users
import qualified Users.Query as Q

main :: IO ()
main = hspec spec

spec :: Spec
spec = withWebServer $ do

  describe "GET /v1/users" $
    it "returns 200 when there are no users" $
      get "/v1/users" `shouldRespondWithJson` (200, emptyUsers)

  describe "GET /v1/users/:id" $
    it "returns 404 when the resource cannot be found" $
      get "/v1/users/1" `shouldRespondWith` 404

  describe "POST /v1/users" $ do
    it "returns 200 when the user is successfully created" $
      postJson "/v1/users" newUserForm `shouldRespondWithJson` (200, newUser)

    it "returns 400 when the user has validation errors" $
      postJson "/v1/users" invalidNewUserForm `shouldRespondWith` 400

  describe "PUT /v1/users/:id" $ do
    it "returns 200 when the resource has been successfully updated" $ do
      AppConfig{..} <- liftIO testAppConfig
      now    <- liftIO Clock.getCurrentTime
      userId <- Q.create (M.User "abc123" "tina" "belcher" now) getDB

      let userForm = UserForm (Just userId) "abc123" "linda" "belcher"
      let updatedUser = User userId "abc123" "linda" "belcher"
      patchJson "/v1/users/abc123" userForm `shouldRespondWithJson` (200, updatedUser)

    it "returns 400 when the resource has validation errors" $
      patchJson "/v1/users/abc123" invalidUserForm `shouldRespondWith` 400

    it "returns 404 when the resource cannot be found" $
      patchJson "/v1/users/xyz789" notFoundUserForm `shouldRespondWith` 404

  describe "DELETE /v1/users/1" $ do
    it "returns 200 when the resource has been successfully deleted" $ do
      AppConfig{..} <- liftIO testAppConfig
      now    <- liftIO Clock.getCurrentTime
      userId <- Q.create (M.User "abc123" "tina" "belcher" now) getDB
      let pathId = BS.pack (show userId)
      delete ("/v1/users/" <> pathId) `shouldRespondWith` 200

    it "returns 200 even when there's no resource to delete" $
      delete "/v1/users/1" `shouldRespondWith` 200


emptyUsers :: [User]
emptyUsers = []

newUserForm :: UserForm
newUserForm = UserForm Nothing "abc123" "bob" "belcher"

notFoundUserForm :: UserForm
notFoundUserForm = UserForm (Just (-1)) "xyz789" "gene" "belcher"

invalidNewUserForm :: UserForm
invalidNewUserForm = UserForm Nothing "" "" ""

invalidUserForm :: UserForm
invalidUserForm = UserForm (Just 1) "" "" ""

newUser :: User
newUser = User 1 "abc123" "bob" "belcher"
