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


runPgSqlComputation :: (EnvironmentVars, PgComputationInput) -> Servant.ClientM PgComputationOutput
runPgSqlComputation =
  Servant.client (Proxy :: Proxy PgSqlComputationApi)


-- * spec


spec :: Spec
spec = do


-- ** nominal cases


  withClient (mkApp defaultEnv) $ do
    describe "valid select query" $ do

      it "returns different integers size" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [Sentence "select 1::smallint as \"a\", 1::integer as \"b\", 1::bigint as \"c\";"]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("a", PgInt 1), ("b", PgInt 1), ("c", PgInt 1) ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns reals" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [Sentence "select 12.345 as \"a\", 12.345::real as \"b\", 12.345::double precision as \"c\", 12.345::decimal as \"d\";"]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("a", PgFloat 12.345), ("b", PgFloat 12.345), ("c", PgFloat 12.345), ("d", PgFloat 12.345) ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output


      it "returns strings" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [Sentence "select \'coucou\' as \"a\", 'coucou'::text as \"b\", 'coucou'::varchar as \"c\";"]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("a", PgString "coucou"), ("b", PgString "coucou"), ("c", PgString "coucou") ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns null" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [Sentence "select null as \"null\";"]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("null", PgNull) ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns bool" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [Sentence "select True as \"bool\";"]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("bool", PgBool True) ] ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns tables" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [Sentence "select * from (values(1,2), (3,4)) as foo(a,b);"]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("a", PgInt 1), ("b", PgInt 2) ]
                                 , Row [ ("a", PgInt 3), ("b", PgInt 4) ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns array as string" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [Sentence "select ARRAY[1,2]::integer array;"]
                validPgConnection
            )
        let output = Right $ PgTuplesOk [ Row [ ("array", PgString "{1,2}") ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "returns tuples as string" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [Sentence "select (1,2);"]
                validPgConnection
            )
        let output = Right $ PgTuplesOk [ Row [ ("row", PgString "(1,2)") ]
                                        ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output


-- ** insert update delete


    describe "insert, update, delete" $ do
      it "inserts" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [ Sentence "insert into user_test (firstname, lastname, role) values ('a', 'b', 'manager');" ]
                validPgConnection
            )
        let output = Right PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "inserts and return table" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [ Sentence "insert into user_test (firstname, lastname, role) values ('a', 'b', 'manager') RETURNING firstname, lastname;"]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("firstname", PgString "a"), ("lastname", PgString "b") ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "updates" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [ Sentence "update user_test set firstname = '' where id = 0;"]
                validPgConnection
            )
        let output = Right PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "deletes" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [ Sentence "delete from user_test where id = 0;"]
                validPgConnection
            )
        let output = Right PgCommandOK
        try clientEnv (runPgSqlComputation input) `shouldReturn` output


-- ** var substitution

    describe "var substitution" $ do
      it "sustitute one var" $ \clientEnv -> do
        let
          input =
            ( EnvironmentVars $ Map.fromList [ ( "name", [ Sentence "John" ]) ]
            , PgComputationInput
                [ Sentence "select '"
                , Key "name"
                , Sentence "';"
                ]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("?column?", PgString "John") ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "sustitute multiple vars" $ \clientEnv -> do
        let
          input =
            ( EnvironmentVars $ Map.fromList [ ( "firstname", [ Sentence "John" ])
                                             , ( "lastname", [ Sentence "Doe" ])
                                             ]
            , PgComputationInput
                [ Sentence "select '"
                , Key "firstname"
                , Sentence "' , '"
                , Key "lastname"
                , Sentence "';"
                ]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("?column?", PgString "John"), ("?column?", PgString "Doe") ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

      it "works when var doesn't exist" $ \clientEnv -> do
        let
          input =
            ( EnvironmentVars $ Map.fromList [ ( "firstname", [ Sentence "John" ])
                                             ]
            , PgComputationInput
                [ Sentence "select '"
                , Key "firstname"
                , Sentence "' , '"
                , Key "lastname"
                , Sentence "';"
                ]
                validPgConnection
            )
        let output =
              Right $ PgTuplesOk [ Row [ ("?column?", PgString "John"), ("?column?", PgString "{{lastname}}") ]
                                 ]
        try clientEnv (runPgSqlComputation input) `shouldReturn` output

-- ** invalid query


    describe "invalid query" $ do
      it "fails on bad syntax" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [ Sentence "selec t;"]
                validPgConnection
            )
        let output = Left $ PgError "FatalError syntax error at or near \"selec\""
        try clientEnv (runPgSqlComputation input) `shouldReturn` output


-- ** invalid connection


    describe "invalid connection" $ do
      it "fails on invalid connection" $ \clientEnv -> do
        let
          input =
            ( emptyEnvironmentVars
            , PgComputationInput
                [ Sentence "select 1;"]
                TemplatedPgConnection { _templatedPgConnectionHost     = [ Sentence "localhosti" ]
                                      , _templatedPgConnectionPort     = [ Sentence "5432" ]
                                      , _templatedPgConnectionUser     = [ Sentence "postgres" ]
                                      , _templatedPgConnectionPassword = [ Sentence "" ]
                                      , _templatedPgConnectionDbName   = [ Sentence "test" ]
                                      }
            )
        let output = Left $ PgError "FatalError: check the pg database connection"
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
