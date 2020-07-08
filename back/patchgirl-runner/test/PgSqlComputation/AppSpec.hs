{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PgSqlComputation.AppSpec where

import qualified Data.Map.Strict        as Map
import           Servant
import qualified Servant.Client         as Servant
import           Test.Hspec

import           Api
import           Helper.App
import           Interpolator
import           PgSqlComputation.Model
import           Server


-- * client


runPgSqlComputation :: (StringTemplate, EnvironmentVars) -> Servant.ClientM PgComputation
runPgSqlComputation =
  Servant.client (Proxy :: Proxy PgSqlComputationApi)


-- * spec


spec :: Spec
spec = do


-- ** computation succeeded


  withClient (mkApp defaultEnv) $ do
    describe "valid select query" $ do
      it "returns string" $ \clientEnv -> do
        let input =
              ( [Sentence "select \'coucou\' as \"string\", 1 as \"integer\", null as \"null\";"]
              , Map.empty
              )
        let output = PgTuplesOk $ Table [ Column "string" [ PgString "coucou" ]
                                        , Column "integer" [ PgInt 1 ]
                                        , Column "null" [ PgNull ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns bool" $ \clientEnv -> do
        let input =
              ( [Sentence "select True as \"bool\";"]
              , Map.empty
              )
        let output = PgTuplesOk $ Table [ Column "bool" [ PgBool True ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns tables" $ \clientEnv -> do
        let input =
              ( [Sentence "select * from (values(1,2), (3,4)) as foo(a,b);"]
              , Map.empty
              )
        let output = PgTuplesOk $ Table [ Column "a" [ PgInt 1, PgInt 3 ]
                                        , Column "b" [ PgInt 2, PgInt 4 ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns tuples as string" $ \clientEnv -> do
        let input =
              ( [Sentence "select (1,2);"]
              , Map.empty
              )
        let output = PgTuplesOk $ Table [ Column "row" [ PgString "(1,2)" ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

    describe "insert, update, delete" $ do
      it "inserts" $ \clientEnv -> do
        let input =
              ( [Sentence "insert into user_test (firstname, lastname) values ('a', 'b');"]
              , Map.empty
              )
        let output = PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "inserts and return table" $ \clientEnv -> do
        let input =
              ( [Sentence "insert into user_test (firstname, lastname) values ('a', 'b') RETURNING firstname, lastname;"]
              , Map.empty
              )
        let output = PgTuplesOk $ Table [ Column "firstname" [ PgString "a" ]
                                        , Column "lastname" [ PgString "b" ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "updates" $ \clientEnv -> do
        let input =
              ( [Sentence "update user_test set firstname = '' where id = 0;"]
              , Map.empty
              )
        let output = PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "deletes" $ \clientEnv -> do
        let input =
              ( [Sentence "delete from user_test where id = 0;"]
              , Map.empty
              )
        let output = PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

    describe "invalid query" $ do
      it "fails on bad syntax" $ \clientEnv -> do
        let input =
              ( [Sentence "selec t;"]
              , Map.empty
              )
        let output = PgError "FatalError"
        try clientEnv (runPgSqlComputation input) `shouldReturn` output
