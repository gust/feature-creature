module Products.Controller
  ( ProductsAPI
  , actions
  , indexA
  , createA
  ) where

import App (AppT, AppConfig (..))
import Control.Monad.Reader
import Data.Time.Clock as Clock
import Errors
import qualified Models as M
import Products.Api (Product (..), ProductForm (..))
import qualified Products.Api as P
import Products.Query as Q
import Servant
import Users.Api (User)
import qualified Users.Api as U

type ProductsAPI = Get '[JSON] [Product]
              :<|> ReqBody '[JSON] ProductForm :> Post '[JSON] Product

actions :: User -> ServerT ProductsAPI AppT
actions user = indexA user :<|> createA user

indexA :: User -> AppT [Product]
indexA _ = ask >>= \AppConfig{..} ->
  fmap (map Q.toApiProduct) (Q.findAll getDB)

createA :: User -> ProductForm -> AppT Product
createA currentUser productForm =
  if P.hasValidationErrors productForm then
    raiseAppError $ BadRequest (P.formValidationErrors productForm)
  else
    createNewProduct currentUser productForm

createNewProduct :: User -> ProductForm -> AppT Product
createNewProduct user ProductForm{..} = ask >>= \AppConfig{..} -> do
  now       <- liftIO Clock.getCurrentTime
  productId <- Q.create (M.Product getRepositoryID (U.id user) getRepositoryName now) getDB
  return $ Product productId getRepositoryName
