{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE ViewPatterns      #-}

module Main where

--import           Data.Default            (def)
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy
import           Yesod.Auth.OAuth2.Google

import           Lib

data Api = Api
  { httpManager :: Manager
  }

mkYesod "Api" [parseRoutes|
/test/#Text TestRoute GET
/test2/#User Test2Route GET
/test3 PermissionDeniedRoute GET
/auth  AuthR  Auth getAuth
/      HomeR  GET POST
/admin AdminR GET
|]

instance Yesod Api where
  authRoute _ = Just $ AuthR LoginR

  -- route name, then a boolean indicating if it's a write request
  isAuthorized HomeR True = isAdmin
  isAuthorized AdminR _   = isAdmin

  -- anyone can access other pages
  isAuthorized _ _        = return Authorized

isAdmin = do
  mu <- maybeAuthId
  return $ case mu of
    Nothing      -> AuthenticationRequired
    Just "admin" -> Authorized
    Just _       -> Unauthorized "You must be an admin"

instance YesodAuth Api where
  type AuthId Api = Text
  authenticate = return . Authenticated . credsIdent

  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins _ = [authDummy]
  maybeAuthId = lookupSession "_ID"

instance RenderMessage Api FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Note: Log in as "admin" to be an administrator.
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            <p>
                <a href=@{AdminR}>Go to admin page
            <form method=post>
                Make a change (admins only)
                \ #
                <input type=submit>
        |]

postHomeR :: Handler ()
postHomeR = do
    setMessage "You made some change to the page"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout
    [whamlet|
        <p>I guess you're an admin!
        <p>
            <a href=@{HomeR}>Return to homepage
    |]

getHomeRoute :: Handler Html
getHomeRoute = defaultLayout [whamlet|Hello World!|]

getTestRoute :: Text -> Handler Html
getTestRoute name = defaultLayout [whamlet|Hello #{name} !|]

newtype User = User Text deriving (Eq, Show, Read)

instance PathPiece User where
  toPathPiece (User name) = T.pack $ show name
  fromPathPiece s = Just $ User s

getTest2Route :: User -> Handler Html
getTest2Route (User name) = do
  foo <- lookupGetParam "test"
  case foo of
    Just param -> defaultLayout [whamlet|Hello #{name}, #{param} !|]
    Nothing    -> invalidArgs ["you are", "so", "stupid"]

getPermissionDeniedRoute :: Handler Html
getPermissionDeniedRoute = permissionDenied "idiot"

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  warp 3000 $ Api manager
