module Users
  ( User (..)
  , UserForm (..)
  , hasValidationErrors
  , formValidationErrors
  , module Users.Api
  ) where

import Data.Text as T
import Users.Api

formValidationErrors :: UserForm -> Text
formValidationErrors form =
  if hasValidationErrors form then
    "First and last name required."
  else
    T.empty

hasValidationErrors :: UserForm -> Bool
hasValidationErrors (UserForm _ authProviderId email name) =
  T.null authProviderId
    || T.null email
    || T.null name
