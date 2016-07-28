module App.Products.Product exposing
  ( Product
  , ProductMsg (..)
  , init
  )

import Http exposing (Error)

type alias Product =
  { id   : Int
  , name : String
  }

type ProductMsg = FetchProductsSucceeded (List Product)
                | FetchProductsFailed Error
                | CreateProductsSucceeded Product
                | CreateProductsFailed Error

init : Int -> String -> Product
init prodID prodName =
  { id   = prodID
  , name = prodName
  }
