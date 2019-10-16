module RequestCollection.HandlerSpec where

import           App
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Network.HTTP.Types
import           RequestCollection                          hiding (getRequests, getRequest, getRequestCollectionById)
import           Servant
import           Servant.Client
import           Test.Hspec
import Helper.DB (cleanDBAfter)
import Helper.App
import qualified RequestCollection.Fixture as Fixture

getRequests :: ClientM [Request]
getRequest :: Int -> ClientM Request
getRequestCollectionById :: Int -> ClientM RequestCollection
getRequests :<|> getRequest :<|> getRequestCollectionById =
  client api

spec :: Spec
spec = do
  describe "GET /requestCollection/:id" $ do
    withClient mkApp $ do
      it "return requestCollection by id" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          let RequestCollection _ requestNodesToInsert = Fixture.requestCollectionSample1
          RequestCollection id insertedRequestNodes <- insertRequestNodes connection requestNodesToInsert
          try clientEnv (getRequestCollectionById id) `shouldReturn` RequestCollection id insertedRequestNodes

      it "return 404 for missing request collection" $ \clientEnv -> do
        try clientEnv (getRequestCollectionById 1) `shouldThrow` errorsWithStatus notFound404
