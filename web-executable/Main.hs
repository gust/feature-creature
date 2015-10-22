{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where
  import Data.Aeson
  import Data.Time.Calendar
  import GHC.Generics
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Servant

  data User = User { name :: String
                   , age :: Int
                   , email :: String
                   , registration_date :: Day
                   } deriving (Eq, Show, Generic)

  -- orphan ToJSON instance for Day. necessary to derive one for User
  {- instance ToJSON Day where -}
    -- display a day in YYYY-mm-dd format
    {- toJSON d = toJSON (showGregorian d) -}

  instance ToJSON User

  type UserAPI = "users" :> Get '[JSON] [User]

  main :: IO ()
  main = run 8081 app

  server :: Server UserAPI
  server = return users

  userAPI :: Proxy UserAPI
  userAPI = Proxy

  -- 'serve' comes from servant and hands you a WAI Application,
  -- which you can think of as an "abstract" web application,
  -- not yet a webserver.
  app :: Application
  app = serve userAPI server

  users :: [User]
  users = 
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]
