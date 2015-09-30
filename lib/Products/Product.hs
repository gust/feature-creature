{-# LANGUAGE OverloadedStrings #-}

module Products.Product where
  import Control.Monad.Trans.Class (MonadTrans)
  import Control.Monad.IO.Class (MonadIO)
  import Database
  import qualified Database.Persist as DB
  import qualified Database.Persist.Postgresql as DB
  import GHC.Int (Int64)
  import Models


  createProduct :: (MonadTrans t, MonadIO (t Database.ConfigM)) =>
       Product -> t Database.ConfigM Int64
  createProduct p = do
    newProduct <- runDB $ DB.insert p
    return $ DB.fromSqlKey newProduct
