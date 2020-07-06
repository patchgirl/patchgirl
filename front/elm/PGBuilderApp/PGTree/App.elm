module PGBuilderApp.PGTree.App exposing (..)

--import PGBuilderApp.PGBuilder.App as PGBuilder

import Animation
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Input as Input
import Http
import Page exposing (..)
import Random
import Util exposing (..)
import Uuid



-- * model


type alias Model a =
    { a
        | environments : List Environment
  {-requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid.Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int-}
    }



-- * message


type Msg
    = ToggleFolder Uuid.Uuid
    | ToggleMenu Uuid.Uuid
      -- mkdir
    | GenerateRandomUUIDForFolder Uuid.Uuid
    | AskMkdir Uuid.Uuid Uuid.Uuid
    | Mkdir Uuid.Uuid Uuid.Uuid
      -- create file
    | GenerateRandomUUIDForFile Uuid.Uuid
    | AskTouch Uuid.Uuid Uuid.Uuid
    | Touch Uuid.Uuid Uuid.Uuid
      -- create root file
    | GenerateRandomUUIDForRootFile
    | AskTouchRoot Uuid.Uuid
    | TouchRoot Uuid.Uuid
      -- create root folder
    | GenerateRandomUUIDForRootFolder
    | AskMkdirRoot Uuid.Uuid
    | MkdirRoot Uuid.Uuid
      -- rename
    | ShowRenameInput Uuid.Uuid
    | ChangeName Uuid.Uuid String -- while focus is on the input
    | AskRename Uuid.Uuid String -- validate input
    | Rename Uuid.Uuid String -- refresh input
      -- delete
    | AskDelete Uuid.Uuid
    | Delete Uuid.Uuid
    | BuilderTreeServerError



-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    (model, Cmd.none)


-- * view


view : Model a -> Element Msg
view model =
    none
