module Repositories.Controller
( RepositoriesAPI
, indexA
) where

import AccessTokens (withAccessToken)
import App (AppT)
import Data.Text (Text)
import Errors (raiseMissingAccessTokenError)
import Repositories (Repository)
import Repositories.Query (fetchUserRepos)
import Servant
import Users.Api

type RepositoriesAPI = RepositoriesIndex

type RepositoriesIndex = Header "Cookie" Text
                       :> Get '[JSON] [Repository]

indexA :: User -> Maybe Text -> AppT [Repository]
indexA _ Nothing        = raiseMissingAccessTokenError
indexA _ (Just cookies) = withAccessToken cookies fetchUserRepos
