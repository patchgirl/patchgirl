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


runPgSqlComputation :: String -> Servant.ClientM (Maybe Table)
runPgSqlComputation =
  Servant.client (Proxy :: Proxy PgSqlComputationApi)


-- * spec


spec :: Spec
spec = do


-- ** computation succeeded


  withClient (mkApp defaultEnv) $ do
    fdescribe "valid request computation input" $ do
      it "returns string" $ \clientEnv -> do
        let expectedRes = Table [ Column "string" [ PGString "coucou" ]
                                , Column "integer" [ PGInt 1 ]
                                , Column "null" [ PGNull ]
                                ]
        try clientEnv (runPgSqlComputation "select \'coucou\' as \"string\", 1 as \"integer\", null as \"null\";") `shouldReturn` Just expectedRes

      it "returns bool" $ \clientEnv -> do
        let expectedRes = Table [ Column "bool" [ PGBool True ]
                                ]
        try clientEnv (runPgSqlComputation "select True as \"bool\";") `shouldReturn` Just expectedRes

      it "returns tables" $ \clientEnv -> do
        let expectedRes = Table [ Column "a" [ PGInt 1, PGInt 3 ]
                                , Column "b" [ PGInt 2, PGInt 4 ]
                                ]
        try clientEnv (runPgSqlComputation "select * from (values(1,2), (3,4)) as foo(a,b);") `shouldReturn` Just expectedRes
