module PGBuilderApp.PGTree.App exposing (..)

import Animation
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Font as Font
import Http
import Page exposing (..)
import Random
import Util exposing (..)
import Uuid exposing (Uuid)
import Browser.Navigation as Navigation
import HttpError exposing(..)
import Element.Events exposing (..)
import BuilderUtil exposing (..)


-- * model


type alias Model a =
    { a
        | pgCollection : PgCollection
        , displayedPgNodeMenuId : Maybe Uuid
        , displayedPgBuilderView : BuilderView Uuid
    }


-- * message


type Msg
    = ToggleMenu (Maybe Uuid)
    | ToggleFolder Uuid


-- * update


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        ToggleMenu mId ->
            { model | displayedPgNodeMenuId = mId }

        ToggleFolder id ->
            let
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgNodes =
                    List.map (modifyPgNode id toggleFolder) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection pgCollectionId newPgNodes
                    }
            in
            newModel


-- * view


view : Model a -> Element Msg
view model =
    let
        (PgCollection _ pgNodes) =
            model.pgCollection
    in
    column [ spacing 10 ] (nodeView model pgNodes)


nodeView : Model a -> List PgNode -> List (Element Msg)
nodeView model pgCollection =
    case pgCollection of
        [] ->
            []

        node :: tail ->
            case node of
                Folder { id, name, open, children } ->
                    let
                        (PgChildren c) =
                            children

                        folderChildrenView =
                            nodeView model c

                        tailView =
                            nodeView model tail

                        currentFolderView =
                            folderView id model name folderChildrenView open
                    in
                    currentFolderView :: tailView

                File { id, name } ->
                    let
                        tailView =
                            nodeView model tail

                        currentFileView =
                            fileView id model name
                    in
                    currentFileView :: tailView



-- ** file view


fileView : Uuid -> Model a -> Editable String -> Element Msg
fileView id model eName =
    let
        showMenu =
            model.displayedPgNodeMenuId == Just id

        name =
            notEditedValue eName

        selected =
            getBuilderId model.displayedPgBuilderView == Just id

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular
    in
    el [ onMouseEnter (ToggleMenu (Just id))
       , onMouseLeave (ToggleMenu Nothing)
       ] <|
        row [ weight ] <|
            [ case showMenu of
                  True ->
                      link []
                          { url = href (PgPage (EditView (DefaultEditView id)))
                          , label =
                              iconWithAttr { defaultIconAttribute
                                               | title = ""
                                               , icon = "edit"
                                           }
                          }

                  False ->
                      iconWithAttr { defaultIconAttribute
                                       | title = ""
                                       , primIconColor = Just color
                                       , icon = "label"
                                   }
            , link [ weight ]
                { url = href (PgPage (RunView id))
                , label = text name
                }
            ]


-- ** folder view


folderView : Uuid -> Model a -> Editable String -> List (Element Msg) -> Bool -> Element Msg
folderView id model eName folderChildrenView open =
    let
        showMenu =
            Just id == model.displayedPgNodeMenuId

        name =
            notEditedValue eName

        selected =
            getBuilderId model.displayedPgBuilderView == Just id

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular

    in
    column []
        [ row [ weight
              , onMouseEnter (ToggleMenu (Just id))
              , onMouseLeave (ToggleMenu Nothing)
              ]
              [ link []
                    { url = href (PgPage (EditView (DefaultEditView id)))
                    , label =
                        iconWithAttr { defaultIconAttribute
                                         | title = ""
                                         , icon =
                                           case showMenu of
                                               True -> "edit"
                                               False ->
                                                   case open of
                                                       False -> "keyboard_arrow_right"
                                                       True -> "keyboard_arrow_down"
                                     }
                    }
              , Input.button [ weight ]
                  { onPress = Just <| ToggleFolder id
                  , label = text name
                  }
              ]
        , case open of
              True ->
                  column [ spacing 10, paddingXY 20 10 ] folderChildrenView

              False ->
                  none
        ]
