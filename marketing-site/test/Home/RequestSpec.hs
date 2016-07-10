module Home.RequestSpec
  ( main
  , spec
  ) where

import Helpers.TestWebServer

main :: IO ()
main = hspec spec

spec :: Spec
spec = withWebServer $ do

  describe "GET /" $
    it "returns 200 when there are no users" $
      get "/" `shouldRespondWith` 200
