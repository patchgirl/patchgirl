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

module Session.AppSpec where

import           Account.Model
import           App
import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader)
import           Data.Functor           ((<&>))
import           Helper.App
import           Helper.DB              (cleanDBAfter)
import           Model
import           Network.HTTP.Types     (badRequest400)
import           PatchGirl
import           Servant
import           Servant                (Header, Headers, Proxy, err400)
import           Servant.Auth.Client
import           Servant.Auth.Server    (Cookie, CookieSettings, JWT,
                                         JWTSettings, SetCookie, fromSecret,
                                         makeJWT)
import           Servant.Client         (ClientM, client)
import           Servant.Server         (ServerError)
import           Session.App
import           Session.Model
import           Test.Hspec

-- * client

signIn
  :: Login
  -> ClientM (Headers '[ Header "Set-Cookie" SetCookie
                       , Header "Set-Cookie" SetCookie]
               Session)
signUp :: SignUp -> ClientM ()
signOut :: ClientM (Headers '[ Header "Set-Cookie" SetCookie
                             , Header "Set-Cookie" SetCookie]
                     Session)
signIn :<|> signUp :<|> signOut =
  client (Proxy :: Proxy SessionApi)


whoAmI
  :: Token
  -> ClientM (Headers '[ Header "Set-Cookie" SetCookie
                       , Header "Set-Cookie" SetCookie
                       ] Session)
whoAmI =
  client (Proxy :: Proxy (PSessionApi '[JWT]))


-- * spec


spec :: Spec
spec = do


-- ** who am i


  describe "who am I" $ do
    withClient (mkApp defaultConfig) $ do
      context "when signed in" $ do
        it "should return a signed user session" $ \clientEnv ->
          cleanDBAfter $ \connection -> do
            token <- signedUserToken 1
            (_, session) <- try clientEnv (whoAmI token) <&> (\r -> (getHeaders r, getResponse r))
            session `shouldBe` SignedUserSession { _sessionAccountId = 1
                                                 , _sessionCsrfToken = ""
                                                 , _sessionEmail = "foo@mail.com"
                                                 }

      context "when unsigned" $ do
        it "should return a signed user session" $ \clientEnv ->
          cleanDBAfter $ \connection -> do
            token <- visitorToken
            (_, session) <- try clientEnv (whoAmI token) <&> (\r -> (getHeaders r, getResponse r))
            session `shouldBe` VisitorSession { _sessionAccountId = 1
                                              , _sessionCsrfToken = ""
                                              }


-- ** sign up


  describe "sign up" $ do
    withClient (mkApp defaultConfig) $ do
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


-- ** sign out


  describe "sign out" $ do
    withClient (mkApp defaultConfig) $ do
      it "always return a visitor session and clean the cookies" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          ([(headerName, headerValue), _], session) <- try clientEnv signOut <&> (\r -> (getHeaders r, getResponse r))
          headerName `shouldBe` "Set-Cookie"
          headerValue `shouldBe` "JWT=value; Path=/; Expires=Tue, 10-Oct-1995 00:00:00 GMT; Max-Age=0; HttpOnly; Secure"
          session `shouldBe` VisitorSession { _sessionAccountId = 1
                                            , _sessionCsrfToken = ""
                                            }
