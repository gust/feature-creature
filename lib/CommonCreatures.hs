module CommonCreatures where
  import Control.Monad.Except (ExceptT)

  type WithErr a = ExceptT String IO a

