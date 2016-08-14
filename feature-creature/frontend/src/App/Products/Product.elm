module App.Products.Product exposing
  ( Product
  , NewProduct
  , ProductMsg (..)
  , init
  , encodeNewProduct
  )

import App.Products.Repository exposing (Repository, encodedRepository)
import Json.Encode
import Http exposing (Error)

type alias Product =
  { id   : Int
  , name : String
  }

type alias NewProduct =
  { repository : Repository
  }

type ProductMsg = FetchProductsSucceeded (List Product)
                | FetchProductsFailed Error
                | CreateProductSucceeded Product
                | CreateProductFailed Error

init : Int -> String -> Product
init prodID prodName =
  { id   = prodID
  , name = prodName
  }

encodeNewProduct : NewProduct -> String
encodeNewProduct newProduct =
  Json.Encode.encode 0
    <| Json.Encode.object
      [ ("repositoryForm", encodedRepository newProduct.repository)
      ]
