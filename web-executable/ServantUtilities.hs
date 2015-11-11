module ServantUtilities where
  import Control.Monad.Trans.Either
  import Servant

  type Handler a           = EitherT ServantErr IO a
