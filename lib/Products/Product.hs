module Products.Product 
  ( Product(Product)
  , ProductID
  , findProducts
  , createProduct
  , updateRepo
  ) where

  import qualified Config as Cfg
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

  updateRepo :: Product -> ProductID -> IO (Either String String)
  updateRepo prod prodId = do
    basePath <- Cfg.gitRepositoryStorePath
    let prodPath = productDir basePath prodId
    createDirectoryIfMissing True prodPath
    updateGitRepo prodPath (repoUrl prod)

  updateGitRepo :: FilePath -> T.Text -> IO (Either String String)
  updateGitRepo path gitUrl = do
    let repoPath = path ++ "repo/"
    doesRepoExist <- doesDirectoryExist repoPath
    case doesRepoExist of
      True  -> undefined -- gitPull repoPath
      False -> Git.clone repoPath gitUrl

  productDir :: FilePath -> Int64 -> FilePath
  productDir basePath prodId = basePath ++ "products/" ++ (show prodId) ++ "/"

  findProducts :: IO [Product]
  findProducts = do
    allProducts <- runDB $ DB.selectList [] []
    return $ map modelToProduct (allProducts :: [DB.Entity ProductModel])

  createProduct :: Product -> IO Int64
  createProduct p = do
    newProduct <- runDB $ DB.insert $ productToModel p
    return $ DB.fromSqlKey newProduct

  productToModel :: Product -> ProductModel
  productToModel p = ProductModel (productName p) (repoUrl p)

  -- FIXME: the ID should be set, if known
  modelToProduct :: DB.Entity ProductModel -> Product
  modelToProduct (DB.Entity _ pm) = Product (productModelName pm) (productModelRepoUrl pm)
