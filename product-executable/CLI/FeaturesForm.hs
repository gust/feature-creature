module CLI.FeaturesForm where
  import Control.Monad.Except (runExceptT)
  import Data.DirectoryTree
  import Data.Tree hiding (drawTree)
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
    prodDir <- P.codeRepositoryDir prodID
    result <- runExceptT (F.getFeatures prodDir)
    either putStrLn (putStrLn . drawTree) result

  getProductFeature :: P.ProductID -> FilePath -> IO ()
  getProductFeature prodID path = do
    prodDir <- P.codeRepositoryDir prodID
    result <- runExceptT (F.getFeature (prodDir ++ path))
    either putStrLn putStrLn result

  showFeaturesCommandUsage :: IO ()
  showFeaturesCommandUsage = putStrLn "*** no documentation provided. sorry.***"

  drawTree :: DirectoryTree -> String
  drawTree  = unlines . draw

  draw :: DirectoryTree -> [String]
  draw (Node x ts0) = (show x) : drawSubTrees ts0
    where
      drawSubTrees [] = []
      drawSubTrees [t] =
          "|" : shift "`- " "   " (draw t)
      drawSubTrees (t:ts) =
          "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

      shift first other = zipWith (++) (first : repeat other)
