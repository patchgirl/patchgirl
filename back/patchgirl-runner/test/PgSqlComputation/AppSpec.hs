{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PgSqlComputation.AppSpec where

import           Servant
import qualified Servant.Client         as Servant
import           Test.Hspec

import           Api
import           Helper.App
import           PgSqlComputation.Model
import           Server


-- * client


runPgSqlComputation :: String -> Servant.ClientM PgComputation
runPgSqlComputation =
  Servant.client (Proxy :: Proxy PgSqlComputationApi)


-- * spec


spec :: Spec
spec = do


-- ** computation succeeded


  withClient (mkApp defaultEnv) $ do
    describe "valid select query" $ do
      it "returns string" $ \clientEnv -> do
        let expectedRes = PgTuplesOk $ Table [ Column "string" [ PgString "coucou" ]
                                             , Column "integer" [ PgInt 1 ]
                                             , Column "null" [ PgNull ]
                                             ]
        try clientEnv (runPgSqlComputation "select \'coucou\' as \"string\", 1 as \"integer\", null as \"null\";") `shouldReturn` expectedRes

      it "returns bool" $ \clientEnv -> do
        let expectedRes = PgTuplesOk $ Table [ Column "bool" [ PgBool True ]
                                             ]
        try clientEnv (runPgSqlComputation "select True as \"bool\";") `shouldReturn` expectedRes

      it "returns tables" $ \clientEnv -> do
        let expectedRes = Table [ Column "a" [ PgInt 1, PgInt 3 ]
                                , Column "b" [ PgInt 2, PgInt 4 ]
                                ]
        try clientEnv (runPgSqlComputation "select * from (values(1,2), (3,4)) as foo(a,b);") `shouldReturn` PgTuplesOk expectedRes

      it "returns tuples as string" $ \clientEnv -> do
        let expectedRes = Table [ Column "row" [ PgString "(1,2)" ]
                                ]
        try clientEnv (runPgSqlComputation "select (1,2);") `shouldReturn` PgTuplesOk expectedRes

    describe "insert, update, delete" $ do
      it "inserts" $ \clientEnv -> do
        try clientEnv (runPgSqlComputation "insert into user_test (firstname, lastname) values ('a', 'b');")
          `shouldReturn` PgCommandOK

      it "inserts and return table" $ \clientEnv -> do
        let expectedRes = PgTuplesOk $ Table [ Column "firstname" [ PgString "a" ]
                                             , Column "lastname" [ PgString "b" ]
                                             ]
        try clientEnv (runPgSqlComputation "insert into user_test (firstname, lastname) values ('a', 'b') RETURNING firstname, lastname;")
          `shouldReturn` expectedRes

      it "updates" $ \clientEnv -> do
        try clientEnv (runPgSqlComputation "update user_test set firstname = '' where id = 0;")
          `shouldReturn` PgCommandOK

      it "deletes" $ \clientEnv -> do
        try clientEnv (runPgSqlComputation "delete from user_test where id = 0;")
          `shouldReturn` PgCommandOK

    describe "invalid query" $ do
      it "fails on bad syntax" $ \clientEnv -> do
        try clientEnv (runPgSqlComputation "selec t;") `shouldReturn` PgError "FatalError"
