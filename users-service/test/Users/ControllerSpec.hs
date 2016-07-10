module Users.ControllerSpec
  ( main
  , spec
  ) where

import Helpers.TestWebServer

import App (AppConfig (..), AppT, runAppT)
import Control.Monad.Except
import Control.Exception
import Data.Either (isRight)
import Data.Time.Clock as Clock
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Errors (resourceNotFound, badRequest)
import GHC.Int (Int64)
import Helpers.DatabaseHelpers
import qualified Models as M
import Servant (ServantErr)
import Users
import Users.Controller
import qualified Users.Query as Q

data MyException = MyException Text
    deriving (Show, Typeable)

instance Exception MyException

mkException :: Show a => a -> MyException
mkException = MyException . T.pack . show

mkUser :: AppConfig -> IO Int64
mkUser AppConfig{..} = do
  now <- Clock.getCurrentTime
  Q.create (M.User "abc123" "bob" "belcher" now) getDB

runTest :: AppConfig -> AppT a -> IO (Either ServantErr a)
runTest cfg f = runExceptT (runAppT cfg f)

resetDatabase :: IO ()
resetDatabase = testAppConfig >>= \AppConfig{..} -> do
  M.runMigrations getDBConn
  truncateDatabase getDBConn

main :: IO ()
main = hspec spec

spec :: Spec
spec = before_ resetDatabase $ do
  describe "usersIndex" $ do
    it "returns a list of Users" $ do
      cfg    <- testAppConfig
      userId <- mkUser cfg
      result <- runTest cfg usersIndex
      case result of
        Left err    -> throw (mkException err)
        Right users -> users `shouldBe` [User userId "abc123" "bob" "belcher"]

    it "returns an empty list when no Users exist" $ do
      cfg    <- testAppConfig
      result <- runExceptT (runAppT cfg usersIndex)
      case result of
        Left err    -> throw (mkException err)
        Right users -> users `shouldBe` []

  describe "usersShow" $ do
    it "returns the specified User" $ do
      cfg    <- testAppConfig
      userId <- mkUser cfg
      result <- runTest cfg (usersShow "abc123")
      case result of
        Left err   -> throw (mkException err)
        Right user -> user `shouldBe` User userId "abc123" "bob" "belcher"

    it "returns an error when the User cannot be found" $ do
      cfg    <- testAppConfig
      result <- runTest cfg (usersShow "xyz789")
      case result of
        Left err   -> err `shouldBe` resourceNotFound
        Right user -> throw (mkException $ "Expected resourceNotFound error got " ++ show user)

  describe "usersCreate" $ do
    it "creates a new User" $ do
      cfg    <- testAppConfig
      -- assert we can create the resource
      result <- runTest cfg (usersCreate (UserForm Nothing "abc123" "linda" "belcher"))
      case result of
        Left err   -> throw (mkException err)
        Right user -> user `shouldBe` User 1 "abc123" "linda" "belcher"

      -- assert we can fetch the resource after creation
      result2 <- runTest cfg (usersShow "abc123")
      case result2 of
        Left err   -> throw (mkException err)
        Right user -> user `shouldBe` User 1 "abc123" "linda" "belcher"

    it "returns an error when the User cannot be created" $ do
      cfg    <- testAppConfig
      let userForm = UserForm Nothing "" "" ""
      result <- runTest cfg (usersCreate userForm)
      case result of
        Left err   -> err `shouldBe` badRequest (formValidationErrors userForm)
        Right user -> throw (mkException $ "Expected badRequest error got " ++ show user)

  describe "usersUpdate" $ do
    it "updates an existing User" $ do
      cfg    <- testAppConfig
      userId <- mkUser cfg
      -- assert we can update the resource
      result <- runTest cfg (usersUpdate "abc123" (UserForm (Just userId) "abc123" "gene" "beelcher"))
      case result of
        Left err   -> throw (mkException err)
        Right user -> user `shouldBe` User 1 "abc123" "gene" "beelcher"

      -- assert the update has been persisted
      result2 <- runTest cfg (usersShow "abc123")
      case result2 of
        Left err   -> throw (mkException err)
        Right user -> user `shouldBe` User 1 "abc123" "gene" "beelcher"

    it "returns an error when the User cannot be updated" $ do
      cfg    <- testAppConfig
      userId <- mkUser cfg
      let userForm = UserForm (Just userId) "" "" ""
      result <- runTest cfg (usersUpdate "abc123" userForm)
      case result of
        Left err   -> err `shouldBe` badRequest (formValidationErrors userForm)
        Right user -> throw (mkException $ "Expected badRequest error got " ++ show user)

  describe "usersDelete" $ do
    it "deletes the specified User" $ do
      cfg    <- testAppConfig
      _ <- mkUser cfg
      -- assert the resource has been created
      result <- runTest cfg (usersShow "abc123")
      isRight result `shouldBe` True
      -- assert we can delete the resource
      result2 <- runTest cfg (usersDelete "abc123")
      case result2 of
        Left err   -> throw (mkException err)
        Right resp -> resp `shouldBe` ()
      -- assert the resource has been deleted
      result3 <- runTest cfg (usersShow "abc123")
      isRight result3 `shouldBe` False

    it "does not return an error if the resource cannot be found" $ do
      cfg    <- testAppConfig
      -- assert the resource does not exist
      result <- runTest cfg (usersShow "xyz789")
      isRight result `shouldBe` False
      -- assert response is successful when the resource cannot be found
      result2 <- runTest cfg (usersDelete "xyz789")
      case result2 of
        Left err   -> throw (mkException err)
        Right resp -> resp `shouldBe` ()
