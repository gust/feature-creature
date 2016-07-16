module Helpers.RequestSpecHelpers
  ( module Test.Hspec
  , delete
  , get
  , matchBody
  , patchJson
  , pending
  , pendingWith
  , postJson
  , putJson
  , shouldRespondWith
  , shouldRespondWithJson
  ) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Network.HTTP.Types.Method (methodPost, methodPatch, methodPut)
import Network.Wai.Test (SResponse)
import Test.Hspec hiding (pending, pendingWith)
import Test.Hspec.Wai
  ( WaiExpectation, WaiSession, delete, get, matchBody, request
  , shouldRespondWith, pending, pendingWith )

shouldRespondWithJson :: (ToJSON a)
                      => WaiSession SResponse
                      -> (Integer, a)
                      -> WaiExpectation
shouldRespondWithJson req (expectedStatus, expectedValue) =
  let matcher = (fromInteger expectedStatus)
                { matchBody = Just $ encode expectedValue }
  in shouldRespondWith req matcher

postJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
postJson path =
  request methodPost path [("Content-Type", "application/json")] . encode

patchJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
patchJson path =
  request methodPatch path [("Content-Type", "application/json")] . encode

putJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
putJson path =
  request methodPut path [("Content-Type", "application/json")] . encode
