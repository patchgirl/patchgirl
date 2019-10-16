{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AppSpec where

import           App
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types (Identifier (..))
import           DB
import           Network.HTTP.Types
import           RequestCollection                          hiding (getRequests)
import           Servant
import           Servant.Client
import           Test.Hspec
import Helper.DB (cleanDBAfter)
import Helper.App

getRequests :: ClientM [Request]
getRequest :: Int -> ClientM Request
getRequests :<|> getRequest =
  client api

insertRequest :: Connection -> String -> IO Int
insertRequest connection requestText = do
  [Only id] <- query connection rawQuery (Only requestText :: Only String)
  return id
  where
    rawQuery = [sql|
                   INSERT INTO request (text)
                   VALUES (?)
                   RETURNING id
                   |]

spec :: Spec
spec = do
  describe "GET /requests/:id" $ do
    withClient mkApp $ do
      it "return request by id" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          id <- insertRequest connection "test"
          try clientEnv (getRequest id) `shouldReturn` Request id "test"

      it "return 404 for missing requests" $ \clientEnv -> do
        try clientEnv (getRequest 1) `shouldThrow` errorsWithStatus notFound404

  describe "GET /requests" $ do
    withClient mkApp $ do
      it "return all requests" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          id1 <- insertRequest connection "test1"
          id2 <- insertRequest connection "test2"
          let expectedRes =
                [ Request id1 "test1"
                , Request id2 "test2"
                ]
          try clientEnv getRequests `shouldReturn` expectedRes
