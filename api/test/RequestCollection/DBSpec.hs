{-# LANGUAGE OverloadedStrings #-}

module RequestCollection.DBSpec where

import           Test.Hspec
import RequestCollection
import qualified RequestCollection.Fixture as Fixture
import Helper.DB (cleanDBAfter)

spec :: Spec
spec = do
  it "select" $ do
    cleanDBAfter $ \connection -> do
      let RequestCollection _ requestNodesToInsert = Fixture.requestCollectionSample1
      RequestCollection id insertedRequestNodes <- insertRequestNodes requestNodesToInsert connection
      mSelectedRequestNodes <- selectRequestCollectionById id connection
      mSelectedRequestNodes `shouldBe` (Just $ RequestCollection id insertedRequestNodes)

  it "insert" $ do
    cleanDBAfter $ \connection -> do
      let RequestCollection _ requestNodesToInsert = Fixture.requestCollectionSample1
      RequestCollection _ insertedRequestNodes <- insertRequestNodes requestNodesToInsert connection
      insertedRequestNodes `shouldBe` requestNodesToInsert
