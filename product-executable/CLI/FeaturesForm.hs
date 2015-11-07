module CLI.FeaturesForm where
  import Control.Monad.Except (runExceptT)
  import Data.Tree (drawTree)
  import qualified Products.Product as P
  import qualified Features.Feature as F
  import Safe (readMay)

  showFeatures :: [String] -> IO ()
  showFeatures [prodID]      = case readMay prodID of
                                 (Just productId) -> listAllProductFeatures productId
                                 Nothing          -> showFeaturesCommandUsage
  showFeatures _             = showFeaturesCommandUsage

  showFeature :: [String] -> IO ()
  showFeature (prodID:path:[]) = case readMay prodID of
                                 (Just productId) -> getProductFeature productId path
                                 Nothing          -> showFeaturesCommandUsage
  showFeature _                = showFeaturesCommandUsage

  listAllProductFeatures :: P.ProductID -> IO ()
  listAllProductFeatures prodID = do
    prodDir <- P.productRepositoryDir prodID
    result <- runExceptT (F.getFeatures prodDir)
    either putStrLn (putStrLn . drawTree) result

  getProductFeature :: P.ProductID -> FilePath -> IO ()
  getProductFeature prodID path = do
    prodDir <- P.productRepositoryDir prodID
    result <- runExceptT (F.getFeature (prodDir ++ path))
    either putStrLn putStrLn result

  showFeaturesCommandUsage :: IO ()
  showFeaturesCommandUsage = putStrLn "*** no documentation provided. sorry.***"

