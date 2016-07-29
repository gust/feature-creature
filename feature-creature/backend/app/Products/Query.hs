module Products.Query
  ( create
  , findAll
  , toApiProduct
  ) where

import Control.Monad.IO.Class
import Data.Int (Int64)
import qualified Database.Persist.Postgresql as DB
import Database.Types
import qualified Models as M
import qualified Products.Api as Api

findAll :: (MonadIO app, Monad m) => DBAccess m -> app [DB.Entity M.Product]
findAll DBAccess{..} =
  liftIO $ runDb selectProducts

create :: (MonadIO app, Monad m) => M.Product -> DBAccess m -> app Int64
create prod DBAccess{..} =
  liftIO $ DB.fromSqlKey <$> runDb (insertProduct prod)

toApiProduct :: DB.Entity M.Product -> Api.Product
toApiProduct productEnt =
  let k = DB.entityKey productEnt
      v = DB.entityVal productEnt
  in Api.Product
      (DB.fromSqlKey k)
      (M.productName v)
