{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Account.AppSpec where

import           Account.App
import           Account.DB
import           Account.Model
import           App
import           Control.Lens             ((^.), (^?))
import           Control.Lens.Combinators (_Just)
import           Control.Monad.Except     (MonadError)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Reader     (MonadReader)
import           Data.Functor             ((<&>))
import           Data.Maybe               (isJust, isNothing)
import           Helper.App
import           Model
import           Network.HTTP.Types       (badRequest400, unauthorized401)
import           PatchGirl
import           Servant
import           Servant                  (Header, Headers, Proxy, err400)
import           Servant.Auth.Client
import           Servant.Auth.Server      (Cookie, CookieSettings, JWT,
                                           JWTSettings, SetCookie, fromSecret,
                                           makeJWT)
import           Servant.Client           (ClientM, client)
import           Servant.Server           (ServerError)
import           Session.App
import           Session.Model
import           Test.Hspec


-- * client


signUp :: SignUp -> ClientM ()
initializePassword :: InitializePassword -> ClientM ()
signUp :<|> initializePassword =
  client (Proxy :: Proxy AccountApi)


-- * spec


spec :: Spec
spec = do
  withClient (mkApp defaultConfig) $ do


-- ** sign up


    describe "sign up" $ do
      it "returns 400 on malformed email" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
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
        cleanDBAfter $ \_ -> do
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
          mAccount <- selectFakeAccount accountId connection
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
          (mAccount >>= ((^. fakeAccountPassword))) `shouldSatisfy` isNothing
          let newPayload =
                initializePasswordPayload { _initializePasswordAccountId = accountId
                                          , _initializePasswordToken = signupToken
                                          , _initializePasswordPassword = "howdy!"
                                          }
          _ <- try clientEnv (initializePassword newPayload)
          mAccount <- selectFakeAccount accountId connection
          let password = (mAccount >>= ((^. fakeAccountPassword)))
          password `shouldSatisfy` isJust
