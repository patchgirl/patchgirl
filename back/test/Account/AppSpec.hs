{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module Account.AppSpec where

import           Account.App
import           Account.DB
import           Account.Model
import           App
import           Control.Lens       ((^.))
import           Data.Maybe         (isJust, isNothing)
import           Helper.App
import           Model
import           Network.HTTP.Types (badRequest400)
import           Servant
import           Servant.Client     (ClientM, client)
import           Session.Model
import           Test.Hspec


-- * client


signUp :: SignUp -> ClientM ()
initializePassword :: InitializePassword -> ClientM ()
signUp :<|> initializePassword =
  client (Proxy :: Proxy AccountApi)


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- ** sign up


    describe "sign up" $ do
      it "returns 400 on malformed email" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          let signupPayload = SignUp { _signUpEmail = CaseInsensitive "whatever" }
          try clientEnv (signUp signupPayload) `shouldThrow` errorsWithStatus badRequest400

      it "returns 400 on already used email" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          let newAccount = NewAccount { _newAccountEmail = CaseInsensitive "foo@mail.com" }
          _ <- insertAccount newAccount connection
          let signupPayload = SignUp { _signUpEmail = CaseInsensitive "foo@mail.com" }
          try clientEnv (signUp signupPayload) `shouldThrow` errorsWithStatus badRequest400


-- ** initialize password


    let email = CaseInsensitive "foo@mail.com"

    let newFakeAccount =
          NewFakeAccountWithoutPassword { _newFakeAccountWithoutPasswordEmail = email }

    let initializePasswordPayload =
          InitializePassword { _initializePasswordAccountId = 1
                             , _initializePasswordPassword = "whatever"
                             , _initializePasswordToken = "whatever"
                             }

    describe "initialize password" $ do
      it "should returns 400 when account is not found" $ \clientEnv ->
        cleanDBAfter $ \_ ->
          try clientEnv (initializePassword initializePasswordPayload) `shouldThrow` errorsWithStatus badRequest400

      it "should returns 400 when signupToken doesnt match account token" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, signupToken) <- insertFakeAccountWithoutPassword newFakeAccount connection
          let newPayload =
                initializePasswordPayload { _initializePasswordAccountId = accountId
                                          , _initializePasswordToken = signupToken <> "notTheSame"
                                          }
          try clientEnv (initializePassword newPayload) `shouldThrow` errorsWithStatus badRequest400

      it "should returns 400 when password is already set" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, signupToken) <- insertFakeAccountWithoutPassword newFakeAccount connection
          let newPayload =
                initializePasswordPayload { _initializePasswordAccountId = accountId
                                          , _initializePasswordToken = signupToken
                                          , _initializePasswordPassword = "howdy!"
                                          }
          _ <- try clientEnv (initializePassword newPayload)
          try clientEnv (initializePassword newPayload) `shouldThrow` errorsWithStatus badRequest400

      it "should set the account password " $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, signupToken) <- insertFakeAccountWithoutPassword newFakeAccount connection
          mAccount <- selectFakeAccount accountId connection
          (mAccount >>= (^. fakeAccountPassword)) `shouldSatisfy` isNothing
          let newPayload =
                initializePasswordPayload { _initializePasswordAccountId = accountId
                                          , _initializePasswordToken = signupToken
                                          , _initializePasswordPassword = "howdy!"
                                          }
          _ <- try clientEnv (initializePassword newPayload)
          mAccount <- selectFakeAccount accountId connection
          let password = mAccount >>= (^. fakeAccountPassword)
          password `shouldSatisfy` isJust
