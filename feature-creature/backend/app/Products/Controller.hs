module Products.Controller
  ( ProductsAPI
  , actions
  , indexA
  , createA
  ) where

import AccessTokens (withAccessToken)
import App (AppT, AppConfig (..))
import Control.Monad.Reader
import Data.Text (Text)
import Data.Time.Clock as Clock
import Errors (AppError (..), raiseAppError, raiseMissingAccessTokenError)
import Messaging
import qualified Models as M
import Products.Api (Product (..), ProductForm (..))
import qualified Products.Api as P
import Products.Query as Q
import qualified Repositories as R
import qualified Repositories.Query as R
import Servant
import Users.Api (User)
import qualified Users.Api as U

import Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Network.AMQP.MessageBus as MB

type ProductsAPI = ProductsIndex
              :<|> CreateProduct

type ProductsIndex = Get '[JSON] [Product]

type CreateProduct = Header "Cookie" Text
                  :> ReqBody '[JSON] ProductForm
                  :> Post '[JSON] Product

-- TODO: Move to shared lib once we have a consumer for
-- this job type
data ProductCreatedJob = ProductCreatedJob Product R.RepositoryForm
  deriving (Show, Eq, Generic)

instance ToJSON ProductCreatedJob
instance FromJSON ProductCreatedJob

actions :: User -> ServerT ProductsAPI AppT
actions user = indexA user :<|> createA user

indexA :: User -> AppT [Product]
indexA _ = ask >>= \AppConfig{..} ->
  fmap (map Q.toApiProduct) (Q.findAll getDB)

createA :: User -> Maybe Text -> ProductForm -> AppT Product
createA _ Nothing _ = raiseMissingAccessTokenError
createA currentUser (Just cookies) productForm =
  if P.hasValidationErrors productForm then
    raiseAppError $ BadRequest (P.formValidationErrors productForm)
  else do
    prod <- createNewProduct currentUser productForm
    _    <- withAccessToken cookies createDeployKey
    _    <- queueNewProductMessage (Job ProductCreated (ProductCreatedJob prod (getRepositoryForm productForm)))
    return prod
  where
    createDeployKey token =
      R.createDeployKey token (getRepositoryForm productForm)

createNewProduct :: User -> ProductForm -> AppT Product
createNewProduct user (ProductForm repo)= ask >>= \AppConfig{..} -> do
  now       <- liftIO Clock.getCurrentTime
  productId <- Q.create (M.Product (R.getRepositoryFormId repo) (U.id user) (R.getRepositoryFormName repo) now) getDB
  return $ Product productId (R.getRepositoryFormName repo)

queueNewProductMessage :: ToJSON a => Job a -> AppT ()
queueNewProductMessage job = ask >>= \AppConfig{..} ->
  liftIO $ MB.withConn getRabbitMQConfig (sendProductCreatedMessage job)

sendProductCreatedMessage :: ToJSON a => Job a -> MB.WithConn ()
sendProductCreatedMessage job =
  MB.produceTopicMessage
    (MB.TopicName "feature_creature.product.created")
    (MB.Message job)
