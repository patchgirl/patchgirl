{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Session.AppSpec where

import           Data.Functor        ((<&>))
import           Servant
import           Servant.Auth.Client
import           Servant.Auth.Server (JWT, SetCookie)
import           Servant.Client      (ClientM, client)
import           Test.Hspec

import           Helper.App
import           PatchGirl.Client
import           PatchGirl.Server


-- * client


signOut :: ClientM ( Headers '[ Header "Set-Cookie" SetCookie
                              , Header "Set-Cookie" SetCookie
                              ] Session
                   )
_ :<|> signOut =
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
spec =
  withClient (mkApp defaultEnv) $ do


-- ** who am i


    describe "who am I" $ do
      context "when signed in" $
        it "should return a signed user session" $ \clientEnv ->
          cleanDBAfter $ \_ -> do
          (token, accountId) <- signedUserToken1
          (_, session) <- try clientEnv (whoAmI token) <&> (\r -> (getHeaders r, getResponse r))
          session `shouldBe` SignedUserSession { _sessionAccountId = accountId
                                               , _sessionCsrfToken = ""
                                               , _sessionGithubEmail = Just "foo@mail.com"
                                               , _sessionGithubAvatarUrl = "https://foo.com/someAvatar.jpg"
                                               }

      context "when unsigned" $
        it "should return a signed user session" $ \clientEnv ->
          cleanDBAfter $ \_ -> do
            (token, accountId) <- visitorToken
            (_, session) <- try clientEnv (whoAmI token) <&> (\r -> (getHeaders r, getResponse r))
            session `shouldBe` VisitorSession { _sessionAccountId = accountId
                                              , _sessionCsrfToken = ""
                                              }


-- ** sign out


    describe "sign out" $
      it "always return a visitor session and clean the cookies" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          ([(headerName, headerValue), _], session) <- try clientEnv signOut <&> (\r -> (getHeaders r, getResponse r))
          headerName `shouldBe` "Set-Cookie"
          headerValue `shouldBe` "JWT=value; Path=/; Expires=Tue, 10-Oct-1995 00:00:00 GMT; Max-Age=0; HttpOnly; Secure"
          session `shouldBe` VisitorSession { _sessionAccountId = visitorId
                                            , _sessionCsrfToken = ""
                                            }
