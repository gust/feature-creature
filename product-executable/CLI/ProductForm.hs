module CLI.ProductForm where
  import qualified CLI.DataFiles as Paths
  import Data.List (intersperse)
  import Data.Text (pack)
  import qualified Products.Product as P

  execProductCommand :: [String] -> IO ()
  execProductCommand (cmd:_) 
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

  updateRepo :: P.Product -> P.ProductID -> IO ()
  updateRepo prod prodId = do
    result <- P.updateRepo prod prodId
    either putStrLn putStrLn result

  showProductCommandUsage :: IO ()
  showProductCommandUsage = Paths.showProductCommandUsageFile

  listAllProducts :: IO ()
  listAllProducts = do
    prods <- P.findProducts
    putStrLn $ concat . (intersperse "\n") $ map show prods
