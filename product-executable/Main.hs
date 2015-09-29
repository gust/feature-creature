module Main where
  import qualified CLI.Base as CLI
  import qualified Database as DB
  import System.Environment (getArgs)

  runApplication :: IO ()
  runApplication = do
    args <- getArgs
    case (CLI.parseCommand args) of
      Nothing    -> CLI.showUsage
      (Just cmd) -> CLI.execCommand cmd

  main :: IO ()
  main = DB.initDB >> runApplication

