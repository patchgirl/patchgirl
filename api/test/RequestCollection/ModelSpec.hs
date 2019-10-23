module RequestCollection.ModelSpec where

import           Test.Hspec
import           Data.Aeson (toJSON)
import qualified RequestCollection.Fixture as Fixture

spec :: Spec
spec = do
  it "decode & encode" $ do
    toJSON Fixture.requestCollectionSample1 `shouldBe` Fixture.requestCollectionSample1AsValue
