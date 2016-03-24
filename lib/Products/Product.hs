module Products.Product
( Product(..)
, ProductID
, createProductWithRepoStatus
, findProducts
, toProduct
, toProductID
) where

import Control.Monad.Reader (ask, liftIO)
import Data.Time.Clock as Clock
import qualified Database.Persist.Postgresql as DB
import Database.Types (WithDBPool (..))
import GHC.Int (Int64)
import Models
import ModelTypes (RepositoryState (..))

type ProductID = Int64

createProductWithRepoStatus :: Product -> RepositoryState -> WithDBPool ProductID
createProductWithRepoStatus p rs =
  createProduct p
    >>= \prodId -> createRepoStatus rs prodId
    >> return (DB.fromSqlKey prodId)

createProduct :: Product -> WithDBPool ProductId
createProduct p = ask >>= liftIO . (DB.runSqlPool (DB.insert p))

createRepoStatus :: RepositoryState -> ProductId -> WithDBPool RepositoryStatusId
createRepoStatus rs pId =
  (liftIO Clock.getCurrentTime) >>= \now ->
    let repoStatus = RepositoryStatus pId rs Nothing now
    in ask >>= liftIO . (DB.runSqlPool (DB.insert repoStatus))

findProducts :: WithDBPool [DB.Entity Product]
findProducts =
  let query = DB.selectList ([] :: [DB.Filter Product]) []
  in ask >>= liftIO . (DB.runSqlPool query)

toProductID :: DB.Entity Product -> ProductID
toProductID dbEntity =
  DB.fromSqlKey . DB.entityKey $ dbEntity

toProduct :: DB.Entity Product -> Product
toProduct dbEntity =
  DB.entityVal dbEntity
