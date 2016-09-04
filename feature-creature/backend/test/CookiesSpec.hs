module CookiesSpec where

import Cookies
import qualified Data.Map as Map
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseCookies" $ do
    it "returns a Map of cookie key/value pairs" $ do
      let sampleCookie = "auth-token=some.long.complicated-string; access-token=some-strange.accesstoken;no-space-given=still.parses" 
      let cookieMap = parseCookies sampleCookie
      Map.lookup "access-token" cookieMap `shouldBe` Just "some-strange.accesstoken"
      Map.lookup "auth-token" cookieMap `shouldBe` Just "some.long.complicated-string"
      Map.lookup "no-space-given" cookieMap `shouldBe` Just "still.parses"
