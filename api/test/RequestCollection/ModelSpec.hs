module RequestCollection.ModelSpec where

import           Test.Hspec
import RequestCollection
import           Data.Aeson
import           Data.Aeson.Text
import qualified RequestCollection.Fixture as Fixture

spec :: Spec
spec = do
  it "decode & encode" $ do
    Fixture.requestColectionSample1AsValue `shouldBe` toJSON Fixture.requestCollectionSample1
