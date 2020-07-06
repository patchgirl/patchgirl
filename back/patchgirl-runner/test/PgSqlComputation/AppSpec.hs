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
import           PgSqlComputation.App
import           PgSqlComputation.Model
import           Server


-- * client


runPgSqlComputation :: String -> Servant.ClientM PGComputation
runPgSqlComputation =
  Servant.client (Proxy :: Proxy PgSqlComputationApi)


-- * spec


spec :: Spec
spec = do


-- ** computation succeeded


  withClient (mkApp defaultEnv) $ do
    fdescribe "valid select query" $ do
      it "returns string" $ \clientEnv -> do
        let expectedRes = PGTuplesOk $ Table [ Column "string" [ PGString "coucou" ]
                                             , Column "integer" [ PGInt 1 ]
                                             , Column "null" [ PGNull ]
                                             ]
        try clientEnv (runPgSqlComputation "select \'coucou\' as \"string\", 1 as \"integer\", null as \"null\";") `shouldReturn` expectedRes

      it "returns bool" $ \clientEnv -> do
        let expectedRes = PGTuplesOk $ Table [ Column "bool" [ PGBool True ]
                                             ]
        try clientEnv (runPgSqlComputation "select True as \"bool\";") `shouldReturn` expectedRes

      it "returns tables" $ \clientEnv -> do
        let expectedRes = Table [ Column "a" [ PGInt 1, PGInt 3 ]
                                , Column "b" [ PGInt 2, PGInt 4 ]
                                ]
        try clientEnv (runPgSqlComputation "select * from (values(1,2), (3,4)) as foo(a,b);") `shouldReturn` PGTuplesOk expectedRes

      it "returns tuples as string" $ \clientEnv -> do
        let expectedRes = Table [ Column "row" [ PGString "(1,2)" ]
                                ]
        try clientEnv (runPgSqlComputation "select (1,2);") `shouldReturn` PGTuplesOk expectedRes

    fdescribe "insert, update, delete" $ do
      it "inserts" $ \clientEnv -> do
        try clientEnv (runPgSqlComputation "insert into user_test (firstname, lastname) values ('a', 'b');")
          `shouldReturn` PGCommandOK

      it "inserts and return table" $ \clientEnv -> do
        let expectedRes = PGTuplesOk $ Table [ Column "firstname" [ PGString "a" ]
                                             , Column "lastname" [ PGString "b" ]
                                             ]
        try clientEnv (runPgSqlComputation "insert into user_test (firstname, lastname) values ('a', 'b') RETURNING firstname, lastname;")
          `shouldReturn` expectedRes

      it "updates" $ \clientEnv -> do
        try clientEnv (runPgSqlComputation "update user_test set firstname = '' where id = 0;")
          `shouldReturn` PGCommandOK

      it "deletes" $ \clientEnv -> do
        try clientEnv (runPgSqlComputation "delete from user_test where id = 0;")
          `shouldReturn` PGCommandOK

    fdescribe "invalid query" $ do
      it "fails on bad syntax" $ \clientEnv -> do
        try clientEnv (runPgSqlComputation "selec t;") `shouldReturn` PGError "FatalError"
