-- | {-# LANGUAGE OverloadedStrings #-}

module RequestNode.DBSpec where

import           Test.Hspec
import RequestNode
import RequestNode.Fixture
import Helper.DB (cleanDBAfter)

spec :: Spec
spec = do
  it "insert new request folder" $ do
    cleanDBAfter $ \connection -> do
      createdRequestFolder <- insertRequestFolder newRequestFolder1 connection
      requestFolderName createdRequestFolder `shouldBe` newRequestFolderName newRequestFolder1

  it "insert new request file" $ do
    cleanDBAfter $ \connection -> do
      createdRequestFile <- insertRequestFile newRequestFile1 connection
      createdRequestFileName createdRequestFile `shouldBe` newRequestFileName newRequestFile1
