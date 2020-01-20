module RequestCollection.ModelSpec where

import           Test.Hspec

spec :: Spec
spec = do
  it "true" $ do
    True `shouldBe` True
