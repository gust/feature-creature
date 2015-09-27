module Main where
  import qualified CLI.Base as CLI
  import System.Environment              (getArgs)

  main :: IO ()
  main = do
    args <- getArgs
    case (CLI.parseCommand args) of
      Nothing    -> CLI.showUsage
      (Just cmd) -> CLI.execCommand cmd
