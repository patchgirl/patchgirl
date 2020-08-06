{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Account.AppSpec where

import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Maybe            as Maybe
import           Data.UUID             (UUID)
import qualified Data.UUID             as UUID
import           Servant
import           Servant.Client        (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Client
import           PatchGirl.Server
import           PgCollection.Sql
import           RequestCollection.Sql


-- * client


defaultAccountId :: UUID
defaultAccountId =
  UUID.fromString "00000000-0000-1000-a000-000000000000" & Maybe.fromJust

resetVisitorAccount :: ClientM ()
resetVisitorAccount =
  client (Proxy :: Proxy AccountApi)


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $


-- ** reset visitor account


    describe "reset visitor account" $
      it "should returns 200" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection } -> do
          res <- try clientEnv resetVisitorAccount
          res `shouldBe` ()
          selectFakeAccount defaultAccountId connection >>= (`shouldSatisfy` Maybe.isJust)

          selectRequestCollectionId defaultAccountId connection  >>= \case
            Nothing -> expectationFailure "Request collection were not reset for the visitor account "
            Just requestCollectionId ->
              selectRequestCollectionAvailable defaultAccountId requestCollectionId connection >>= (`shouldBe` True)

          selectPgCollectionId defaultAccountId connection  >>= \case
            Nothing -> expectationFailure "Pg collection were not reset for the visitor account "
            Just pgCollectionId ->
              selectPgCollectionAvailable defaultAccountId pgCollectionId connection >>= (`shouldBe` True)
