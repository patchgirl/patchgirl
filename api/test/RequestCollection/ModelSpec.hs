module RequestCollection.ModelSpec where

import           Test.Hspec
import RequestCollection
import           Data.Aeson
import           Data.Aeson.Text
import RequestCollection.Fixture
import Data.Text.Lazy.Encoding

spec :: Spec
spec = do
  describe "RequestCollection model" $ do
    it "decode & encode" $
      let
        expectedRes :: Maybe Value
        expectedRes = decode $ encodeUtf8 requestCollectionSample1AsText
      in
        expectedRes `shouldBe` Just (toJSON requestCollectionSample1)
