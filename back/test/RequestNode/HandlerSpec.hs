{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module RequestNode.HandlerSpec where

import           Test.Hspec

{-
import           App
import           AppHealth
import           Helper.App
import           Helper.DB                 (cleanDBAfter)
import           Network.HTTP.Types
import qualified RequestCollection.Fixture as Fixture
import           Servant
import           Servant.Client

{-createRequestFile :: Int -> NewRequestFile -> ClientM Int
updateRequestFile :: Int -> Int -> ClientM Int
createRequestFile :<|> updateRequestFile =
  client requestFileApiProxy
-}

spec :: Spec
spec = do
  describe "GET /requestCollection/:id/requestNodes" $ do
    it "true" $ do
      True `shouldBe` True
    {-withClient mkApp $ do
      it "creates request file" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
        let RequestCollection _ requestNodesToInsert = Fixture.requestCollectionSample1
        RequestCollection id insertedRequestNodes <- insertRequestNodes requestNodesToInsert connection
        try clientEnv (getRequestCollectionById id) `shouldReturn` RequestCollection id insertedRequestNodes-}
-}

spec :: Spec
spec =
  it "true" $
    True `shouldBe` True
