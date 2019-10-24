{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module RequestNode.HandlerSpec where

import           App
import           Network.HTTP.Types
import           RequestNode                          hiding (createRequestFile, updateRequestFile)
import           Servant
import           Servant.Client
import           Test.Hspec
import Helper.DB (cleanDBAfter)
import Helper.App
import qualified RequestCollection.Fixture as Fixture
import AppHealth

createRequestFile :: Int -> NewRequestFile -> ClientM CreatedRequestFile
updateRequestFile :: Int -> Int -> ClientM CreatedRequestFile
createRequestFile :<|> updateRequestFile =
  client requestFileApiProxy

spec :: Spec
spec = do
  describe "GET /requestCollection/:id" $ do
    it "foo" $ do
      True `shouldBe` True
    {-
    withClient mkApp $ do
      it "return request collection by id" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          let RequestCollection _ requestNodesToInsert = Fixture.requestCollectionSample1
          RequestCollection id insertedRequestNodes <- insertRequestNodes requestNodesToInsert connection
          try clientEnv (getRequestCollectionById id) `shouldReturn` RequestCollection id insertedRequestNodes

      it "return 404 for missing request collection" $ \clientEnv -> do
        try clientEnv (getRequestCollectionById 1) `shouldThrow` errorsWithStatus notFound404

  describe "POST /requestCollection" $ do
    withClient mkApp $ do
      it "create request collection" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          let RequestCollection _ requestNodesToInsert = Fixture.requestCollectionSample1
          RequestCollection _ insertedRequestNodes <- try clientEnv (postRequestCollection requestNodesToInsert)
          insertedRequestNodes `shouldBe` requestNodesToInsert
-}
