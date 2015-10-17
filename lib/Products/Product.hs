module Products.Product 
  ( Product(Product)
  , ProductID
  , findProducts
  , createProduct
  , updateRepo
  , productRepositoryDir
  ) where

  import CommonCreatures (WithErr)
  import qualified Config as Cfg
  import Control.Applicative ((<$>))
  import Control.Monad.IO.Class (liftIO)
  import qualified Data.Text as T
  import Database (runDB)
  import qualified Database.Persist.Postgresql as DB
  import GHC.Int (Int64)
  import qualified Git
  import Models
  import System.Directory (doesDirectoryExist, createDirectoryIfMissing)

  data Product = Product { productName :: T.Text
                         , repoUrl :: T.Text
                         } deriving (Show)

  type ProductID = Int64

  productDir :: ProductID -> IO FilePath
  productDir prodID = (++ productDirectory) <$> Cfg.gitRepositoryStorePath
    where
      productDirectory = "products/" ++ (show prodID)

  productRepositoryDir :: ProductID -> IO FilePath
  productRepositoryDir prodID = (++ "/repo") <$> (productDir prodID)

  updateRepo :: Product -> ProductID -> WithErr String
  updateRepo prod prodID = do
    prodRepoPath <- liftIO $ productRepositoryDir prodID
    (liftIO $ createRequiredDirectories prodID) >> updateGitRepo prodRepoPath (repoUrl prod)

  createRequiredDirectories :: ProductID -> IO ()
  createRequiredDirectories prodID = productDir prodID >>= createDirectoryIfMissing True

  updateGitRepo :: FilePath -> T.Text -> WithErr String
  updateGitRepo repoPath gitUrl = do
    doesRepoExist <- liftIO $ doesDirectoryExist repoPath
    case doesRepoExist of
      True  -> Git.pull repoPath
      False -> Git.clone repoPath gitUrl

  findProducts :: IO [Product]
  findProducts = do
    allProducts <- runDB $ DB.selectList [] []
    return $ map modelToProduct (allProducts :: [DB.Entity ProductModel])

  createProduct :: Product -> IO ProductID
  createProduct p = do
    newProduct <- runDB $ DB.insert $ productToModel p
    return $ DB.fromSqlKey newProduct

  productToModel :: Product -> ProductModel
  productToModel p = ProductModel (productName p) (repoUrl p)

  modelToProduct :: DB.Entity ProductModel -> Product
  modelToProduct (DB.Entity _ pm) = Product (productModelName pm) (productModelRepoUrl pm)
