module App.Products.ProductForm exposing
  ( ProductForm
  , ProductFormMsg (..)
  , mkProductForm
  , update
  )

import App.AppConfig exposing (AppConfig)
import App.AppConfig as Cfg
import App.Products.Repository exposing (Repository, RepositoryMsg (..))
import App.Products.Repository as R
import Data.External exposing (..)

type alias ProductForm =
  { repositories : External (List Repository)
  , selectedRepository : Maybe Repository
  }

type ProductFormMsg = InitializeForm
                    | RepositorySelected Repository
                    | RepositoryMsg R.RepositoryMsg

update : ProductFormMsg -> ProductForm -> AppConfig -> (ProductForm, Cmd ProductFormMsg)
update msg pForm appConfig = case msg of
  InitializeForm ->
    let cmd = Cmd.map RepositoryMsg <| R.getRepositories appConfig
    in (pForm, cmd)

  RepositorySelected repository ->
    ({ pForm | selectedRepository = Just repository }, Cmd.none)

  (RepositoryMsg (R.FetchRepositoriesSucceeded rs)) ->
    ({ pForm | repositories = Loaded rs }, Cmd.none)

  (RepositoryMsg (R.FetchRepositoriesFailed err)) ->
    let rs = LoadedWithError "Failed to load repositories!"
        result = ({ pForm | repositories = rs }, Cmd.none)
    in Cfg.logMsg' appConfig ("Error: " ++ toString err) result

{-|
  Creates a default ProductForm
-}
mkProductForm : ProductForm
mkProductForm =
  ProductForm NotLoaded Nothing
