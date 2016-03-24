module Products.Product
( Product(..)
, ProductID
, createProduct
, findProducts
, toProduct
, toProductID
) where

import Config.Config (DBConfig, getPool)
import Control.Monad.Reader (ask, liftIO)
import qualified Database.Persist.Postgresql as DB
import Database.Types (WithDBPool (..))
import GHC.Int (Int64)
import Models

type ProductID = Int64

createProduct :: Product -> WithDBPool ProductID
createProduct p = ask
  >>= liftIO . (DB.runSqlPool (DB.insert p))
  >>= return . DB.fromSqlKey

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
