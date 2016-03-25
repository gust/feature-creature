module Products.Product
( Product(..)
, ProductID
, createProductWithRepoStatus
, findProducts
, findProduct
, updateProductRepoState
, toProduct
, toProductID
) where

import CommonCreatures (WithErr)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, liftIO)
import Data.Text (Text)
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

findProduct :: ProductId -> WithDBPool (Maybe Product)
findProduct prodId = ask >>= liftIO . (DB.runSqlPool (DB.get prodId))

updateProductRepoState :: ProductId -> RepositoryState -> Maybe Text -> WithDBPool (WithErr ())
updateProductRepoState prodId state err = (findRepoStatus prodId) >>= \repoStatus ->
  case repoStatus of
    Nothing   -> return $ throwError $ "No RepositoryStatus found for ProductId " ++ (show prodId)
    (Just rs) -> ask >>= \pool ->
      let updates = [ RepositoryStatusState DB.=. state
                    , RepositoryStatusError DB.=. err
                    ]
          query = DB.update (toRepoStatusID rs) updates
      in return $ liftIO (DB.runSqlPool query pool)

findRepoStatus :: ProductId -> WithDBPool (Maybe (DB.Entity RepositoryStatus))
findRepoStatus prodId = ask >>= liftIO . (DB.runSqlPool (DB.getBy (UniqProductId prodId)))

toProductID :: DB.Entity Product -> ProductID
toProductID = DB.fromSqlKey . DB.entityKey

toRepoStatusID :: DB.Entity RepositoryStatus -> RepositoryStatusId
toRepoStatusID = DB.entityKey

toProduct :: DB.Entity Product -> Product
toProduct dbEntity = DB.entityVal dbEntity
