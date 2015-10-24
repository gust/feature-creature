module CLI.DomainTermsForm(execDomainTermsCommand) where
  import qualified CLI.DataFiles as Paths
  import Data.Text(pack)
  import Data.Int(Int64)
  import Data.List (intersperse)
  import Models(ProductId(..), toKey, DomainTerm, Entity)
  import qualified DomainTerms.DomainTerm as DomainTerm

  execDomainTermsCommand :: [String] -> IO ()
  execDomainTermsCommand ("add":args) = showCreateDomainTermForm
  execDomainTermsCommand ("list":[]) = listDomainTerms Nothing
  execDomainTermsCommand ("list":productIdStr:[]) = listDomainTerms $ Just productIdStr
  execDomainTermsCommand _ = showDomainTermsCommandUsage

  showCreateDomainTermForm :: IO ()
  showCreateDomainTermForm = do 
    productId   <- (putStrLn "What is the id of the product this is for? ") >> getLine
    name        <- (putStrLn "What is the name of the term you want to define? ") >> getLine
    description <- (putStrLn "Describe the term: ") >> getLine
    domainTermId  <- DomainTerm.createDomainTerm $ DomainTerm.DomainTerm (strToProductId productId) (pack name) (pack description)
    putStrLn $ "Successfully created Domain Term!" ++ show domainTermId

  showDomainTermsCommandUsage :: IO ()
  showDomainTermsCommandUsage = Paths.showDomainTermsCommandUsageFile

  listDomainTerms :: Maybe String ->  IO ()
  listDomainTerms maybeProductIdStr = findDomainTerms maybeProductIdStr >>= printRoles
    where
      findDomainTerms :: Maybe String -> IO [Entity DomainTerm]
      findDomainTerms (Just productIdStr) = DomainTerm.findByProductId $ (strToProductId $ productIdStr)
      findDomainTerms Nothing = DomainTerm.findDomainTerms

      printRoles roles = putStrLn $ concat . (intersperse "\n") $ map show roles

  strToProductId :: String -> ProductId
  strToProductId = toKey . read
