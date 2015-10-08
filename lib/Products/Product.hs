module Products.Product where
  import qualified Data.Text as T
  import Database (runDB)
  import qualified Database.Persist.Postgresql as DB
  import GHC.Int (Int64)
  import Models

  data Product = Product { productName :: T.Text 
                         } deriving (Show)

  findProducts :: IO [Product]
  findProducts = do
    allProducts <- runDB $ DB.selectList [] []
    return $ map modelToProduct (allProducts :: [DB.Entity ProductModel])

  createProduct :: Product -> IO Int64
  createProduct p = do
    newProduct <- runDB $ DB.insert $ productToModel p
    return $ DB.fromSqlKey newProduct

  productToModel :: Product -> ProductModel
  productToModel p = ProductModel (productName p)

  modelToProduct :: DB.Entity ProductModel -> Product
  modelToProduct (DB.Entity _ pm) = Product (productModelName pm)
