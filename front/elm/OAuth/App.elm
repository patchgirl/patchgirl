module OAuth.App exposing (..)

import Application.Type exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import ViewUtil exposing (..)
import Api.Generated as Client
import Api.Converter as Client
import Http as Http
import Maybe.Extra as Maybe
import Uuid


-- * model


type alias Model a =
    { a | i: Int
    }


-- * message


type Msg
    = ChangeEmailSignIn String
    | ChangePasswordSignIn String
    | AskSignIn
    | SignInSucceed Session
    | SignInFailed


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        _ ->
            Debug.todo "unreachable state: sign in"


-- * util



-- * view


view : Model a -> Element Msg
view model =
    none
