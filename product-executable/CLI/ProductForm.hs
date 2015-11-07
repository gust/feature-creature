module CLI.ProductForm where
  import qualified CLI.DataFiles as Paths
  import Control.Monad.Except (runExceptT)
  import Data.List (intersperse)
  import Data.Text (pack)
  import qualified CLI.FeaturesForm as FF
  import qualified Products.Product as P

  execProductCommand :: [String] -> IO ()
  execProductCommand (cmd:args)
    | cmd == "features" = FF.showFeatures args
    | cmd == "feature"  = FF.showFeature args
    | cmd == "list"     = listAllProducts
    | cmd == "add"      = showCreateProductForm
    | otherwise         = showProductCommandUsage
  execProductCommand [] = showProductCommandUsage

  showCreateProductForm :: IO ()
  showCreateProductForm = do
    prodName    <- (putStrLn "Project Name: ") >> getLine
    prodRepoUrl <- (putStrLn "Git repository url: ") >> getLine
    prodId      <- P.createProduct $ P.Product (pack prodName) (pack prodRepoUrl)

    let message = "Product " ++ (show prodId) ++ " created!"
    let prod = P.Product (pack prodName) (pack prodRepoUrl)
    updateRepo prod prodId >> putStrLn message
    where
      updateRepo :: P.Product -> P.ProductID -> IO ()
      updateRepo prod prodId = do
        result <- runExceptT (P.updateRepo prod prodId)
        either putStrLn putStrLn result

  showProductCommandUsage :: IO ()
  showProductCommandUsage = Paths.showProductCommandUsageFile

  listAllProducts :: IO ()
  listAllProducts = do
    prods <- P.findProducts
    putStrLn $ concat . (intersperse "\n") $ map show prods
