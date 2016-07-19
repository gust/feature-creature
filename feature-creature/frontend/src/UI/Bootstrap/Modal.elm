module UI.Bootstrap.Modal exposing
  ( modal )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E

modal : Html a -> Html a -> Html a -> a -> Html a
modal title body footer closeMsg =
  let
    close = E.onClick closeMsg
  in
    H.div [A.attribute "class" "modal fade in", A.attribute "tabindex" "-1", A.attribute "role" "dialog"] [
      H.div [A.attribute "class" "modal-backdrop fade in", close] []
      , H.div [A.attribute "class" "modal-dialog", A.attribute "role" "document"] [
        H.div [A.attribute "class" "modal-content"] [
          H.div [A.attribute "class" "modal-header"] [
            H.button [A.attribute "type" "button", A.attribute "class" "close", A.attribute "aria-label" "Close", close] [
              H.span [A.attribute "aria-hidden" "true"] [H.text "×"]
            ]
            , H.h4 [A.attribute "class" "modal-title"] [title]
          ]
          , H.div [A.attribute "class" "modal-body"] [body]
          , H.div [A.attribute "class" "modal-footer"] [footer]
        ]
      ]
    ]
