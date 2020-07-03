{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PgSqlComputation.AppSpec where

import           Servant
import qualified Servant.Client       as Servant
import           Test.Hspec

import           Api
import           Helper.App
import           PgSqlComputation.App
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
--      it "returns ok1" $ \clientEnv ->
--        try clientEnv (runPgSqlComputation "select 1 + 1 as \"test\";") `shouldReturn` Just (Table [ Column "test" ["2"] ])

      it "returns ok2" $ \clientEnv -> do
        let expectedRes = Table [ Column "id" [ "00000000-0000-1000-a000-000000000000", "0", "visitor@patchgirl.io" ] ]
        try clientEnv (runPgSqlComputation "SELECT * from account limit 1;") `shouldReturn` Just expectedRes

      it "returns ok2" $ \clientEnv -> do
        let expectedRes = Table [ Column "a" [ "1", "3" ]
                                , Column "b" [ "2", "4" ]
                                ]
        try clientEnv (runPgSqlComputation "select * from (values(1,2), (3,4)) as foo(a,b);") `shouldReturn` Just expectedRes
