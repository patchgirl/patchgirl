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


runPgSqlComputation :: PgComputationInput -> Servant.ClientM PgComputation
runPgSqlComputation =
  Servant.client (Proxy :: Proxy PgSqlComputationApi)


-- * spec


spec :: Spec
spec = do


-- ** nominal cases


  withClient (mkApp defaultEnv) $ do
    describe "valid select query" $ do
      it "returns string" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [Sentence "select \'coucou\' as \"string\", 1 as \"integer\", null as \"null\";"]
              Map.empty
              validPgConnection

        let output = PgTuplesOk $ Table [ Column "string" [ PgString "coucou" ]
                                        , Column "integer" [ PgInt 1 ]
                                        , Column "null" [ PgNull ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns bool" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [Sentence "select True as \"bool\";"]
              Map.empty
              validPgConnection
        let output = PgTuplesOk $ Table [ Column "bool" [ PgBool True ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns tables" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [Sentence "select * from (values(1,2), (3,4)) as foo(a,b);"]
              Map.empty
              validPgConnection
        let output = PgTuplesOk $ Table [ Column "a" [ PgInt 1, PgInt 3 ]
                                        , Column "b" [ PgInt 2, PgInt 4 ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns tuples as string" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [Sentence "select (1,2);"]
              Map.empty
              validPgConnection
        let output = PgTuplesOk $ Table [ Column "row" [ PgString "(1,2)" ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output


-- ** insert update delete


    describe "insert, update, delete" $ do
      it "inserts" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [Sentence "insert into user_test (firstname, lastname) values ('a', 'b');"]
              Map.empty
              validPgConnection
        let output = PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "inserts and return table" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [ Sentence "insert into user_test (firstname, lastname) values ('a', 'b') RETURNING firstname, lastname;"]
              Map.empty
              validPgConnection
        let output = PgTuplesOk $ Table [ Column "firstname" [ PgString "a" ]
                                        , Column "lastname" [ PgString "b" ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "updates" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [ Sentence "update user_test set firstname = '' where id = 0;"]
              Map.empty
              validPgConnection
        let output = PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "deletes" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [ Sentence "delete from user_test where id = 0;"]
              Map.empty
              validPgConnection
        let output = PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output


-- ** var substitution

    describe "var substitution" $ do
      it "sustitute one var" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [ Sentence "select '"
              , Key "name"
              , Sentence "';"
              ]
              (Map.fromList [ ( "name", [ Sentence "John" ]) ])
              validPgConnection
        let output = PgTuplesOk $ Table [ Column "?column?" [ PgString "John"  ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "sustitute multiple vars" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [ Sentence "select '"
              , Key "firstname"
              , Sentence "' , '"
              , Key "lastname"
              , Sentence "';"
              ]
              (Map.fromList [ ( "firstname", [ Sentence "John" ])
                            , ( "lastname", [ Sentence "Doe" ])
                            ])
              validPgConnection
        let output = PgTuplesOk $ Table [ Column "?column?" [ PgString "John"  ]
                                        , Column "?column?" [ PgString "Doe"  ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "works when var doesn't exist" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [ Sentence "select '"
              , Key "firstname"
              , Sentence "' , '"
              , Key "lastname"
              , Sentence "';"
              ]
              (Map.fromList [ ( "firstname", [ Sentence "John" ])
                            ])
              validPgConnection
        let output = PgTuplesOk $ Table [ Column "?column?" [ PgString "John"  ]
                                        , Column "?column?" [ PgString "{{lastname}}"  ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

-- ** invalid query


    describe "invalid query" $ do
      it "fails on bad syntax" $ \clientEnv -> do
        let
          input =
            PgComputationInput
              [ Sentence "selec t;"]
              Map.empty
              validPgConnection
        let output = PgError "FatalError syntax error at or near \"selec\""
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

  where
    validPgConnection :: TemplatedPgConnection
    validPgConnection =
      TemplatedPgConnection { _templatedPgConnectionHost     = [ Sentence "localhost" ]
                            , _templatedPgConnectionPort     = [ Sentence "5432" ]
                            , _templatedPgConnectionUser     = [ Sentence "postgres" ]
                            , _templatedPgConnectionPassword = [ Sentence "" ]
                            , _templatedPgConnectionDbName   = [ Sentence "test" ]
                            }
