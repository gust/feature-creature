module Users.Api
  ( User (..)
  , UserForm (..)
  , Config (..)
  , createUser
  , findUser
  ) where

import Control.Exception
import Control.Lens
import Control.Monad.Except (ExceptT, liftIO, throwError)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Swagger
  ( ToSchema
  , declareNamedSchema
  , defaultSchemaOptions
  , description
  , example
  , genericDeclareNamedSchema
  , schema
  )
import Data.Text as T
import Data.Text.Encoding as T
import GHC.Generics
import GHC.Int (Int64)
import qualified Network.HTTP.Client as HTTP
import Network.Wreq


data User = User
  { id             :: Int64
  , authProviderId :: Text
  , email          :: Text
  , name           :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

data UserForm = UserForm
  { userFormId             :: Maybe Int64
  , userFormAuthProviderId :: Text
  , userFormEmail          :: Text
  , userFormName           :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON UserForm
instance FromJSON UserForm

instance ToSchema User where
  declareNamedSchema p = genericDeclareNamedSchema defaultSchemaOptions p
    & mapped.schema.description ?~ "This is a User. I think you'll be friends"
    & mapped.schema.example ?~ toJSON (User 1 "auth|abcdef123456" "bob@belcher.com" "Bob Belcher")

instance ToSchema UserForm where
  declareNamedSchema p = genericDeclareNamedSchema defaultSchemaOptions p
    & mapped.schema.description ?~ "This is a UserForm. Use this to create new friends"
    & mapped.schema.example ?~ toJSON (UserForm (Just 1) "auth|abcdef123456" "tina@belcher.com" "Tina Belcher")

data Config = Config { basePath :: Text }
  deriving (Show)

findUser :: Text -> Config -> ExceptT Text IO User
findUser authId Config{..} = do
  eResp <- liftIO $ try $ get (unpack $ basePath <> "/v1/users/" <> authId)
  case eResp of
    (Left (HTTP.StatusCodeException status _ _)) -> throwError $ decodeUtf8 (status ^. statusMessage)
    (Left ex) -> liftIO $ throwIO ex
    (Right resp) -> do
      case eitherDecode (resp ^. responseBody) of
        (Left err) -> throwError $ pack $ show err
        (Right user) -> return user

createUser :: UserForm -> Config -> ExceptT Text IO User
createUser userForm Config{..} = do
  eResp <- liftIO $ try $ postWith opts (unpack $ basePath <> "/v1/users") (toJSON userForm)
  case eResp of
    (Left (HTTP.StatusCodeException status _ _)) -> throwError $ decodeUtf8 (status ^. statusMessage)
    (Left ex) -> liftIO $ throwIO ex
    (Right resp) -> do
      case eitherDecode (resp ^. responseBody) of
        (Left err) -> throwError $ pack $ show err
        (Right user) -> return user
  where
    opts = defaults
             & header "Content-Type" .~ ["application/json"]
             & header "Accept" .~ ["application/json"]
