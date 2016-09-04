module Documentation.Controller
  ( documentationIndex
  ) where

import App (AppT)
import Control.Lens
import Data.Proxy (Proxy)
import Data.Swagger
import Servant.Swagger

documentationIndex :: HasSwagger a => Proxy a -> AppT Swagger
documentationIndex api = return $ toSwagger api
  & info.title   .~ "Users API"
  & info.version .~ "0.1"
  & info.description ?~ "The Users API"
  & info.license ?~ ("BSD3" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")
