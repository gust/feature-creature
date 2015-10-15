module CLI.FeaturesForm where
  import Control.Monad.Except (runExceptT)
  import Data.Tree (drawTree)
  import qualified Products.Product as P
  import qualified Features.Feature as F
  import Safe (readMay)

  showFeatures :: [String] -> IO ()
  showFeatures [prodId]    = case readMay prodId of
                               (Just productId) -> listAllProductFeatures productId
                               Nothing   -> showFeaturesCommandUsage
  showFeatures _           = showFeaturesCommandUsage

  listAllProductFeatures :: P.ProductID -> IO ()
  listAllProductFeatures prodID = do
    prodDir <- P.productRepositoryDir prodID
    result <- runExceptT (F.getFeatures prodDir)
    either putStrLn (putStrLn . drawTree) result

  showFeaturesCommandUsage :: IO ()
  showFeaturesCommandUsage = undefined

