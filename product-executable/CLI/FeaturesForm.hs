module CLI.FeaturesForm where
  import qualified Products.Product as P
  import Safe (readMay)

  showFeatures :: [String] -> IO ()
  showFeatures [prodId]    = case readMay prodId of
                               (Just productId) -> listAllProductFeatures productId
                               Nothing   -> showFeaturesCommandUsage
  showFeatures _           = showFeaturesCommandUsage

  listAllProductFeatures :: P.ProductID -> IO ()
  listAllProductFeatures = undefined

  showFeaturesCommandUsage :: IO ()
  showFeaturesCommandUsage = undefined

