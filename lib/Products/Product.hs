module Products.Product where
  import Data.Text
  import Database (runDB)
  import qualified Database.Persist.Postgresql as DB
  import GHC.Int (Int64)
  import Models

  data Product = Product { name :: Text }

  createProduct :: Product -> IO Int64
  createProduct p = do
    newProduct <- runDB $ DB.insert $ productToModel p
    return $ DB.fromSqlKey newProduct

  productToModel :: Product -> ProductModel
  productToModel p = ProductModel (name p)
