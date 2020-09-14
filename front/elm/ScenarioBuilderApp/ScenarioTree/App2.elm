module ScenarioBuilderApp.ScenarioTree.App2 exposing (..)

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
        | scenarioCollection : ScenarioCollection
        , displayedScenarioNodeMenuId : Maybe Uuid
        , displayedScenarioBuilderView : RichBuilderView Uuid (Maybe Uuid)
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
            { model | displayedScenarioNodeMenuId = mId }

        ToggleFolder id ->
            let
                (ScenarioCollection scenarioCollectionId scenarioNodes) =
                    model.scenarioCollection

                newScenarioNodes =
                    List.map (modifyScenarioNode id toggleFolder) scenarioNodes

                newModel =
                    { model
                        | scenarioCollection =
                            ScenarioCollection scenarioCollectionId newScenarioNodes
                    }
            in
            newModel


-- * view


view : Model a -> Element Msg
view model =
    let
        (ScenarioCollection _ scenarioNodes) =
            model.scenarioCollection
    in
    column [ spacing 10 ] (nodeView model scenarioNodes)


nodeView : Model a -> List ScenarioNode -> List (Element Msg)
nodeView model scenarioCollection =
    case scenarioCollection of
        [] ->
            []

        node :: tail ->
            case node of
                Folder { id, name, open, children } ->
                    let
                        (ScenarioChildren c) =
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
            model.displayedScenarioNodeMenuId == Just id

        folderMenuView : Element Msg
        folderMenuView =
            case showMenu of
                True ->
                    link []
                        { url = href (ScenarioPage (RichEditView (DefaultEditView id)))
                        , label = editIcon
                        }

                False ->
                    none

        name =
            notEditedValue eName

        selected =
            getRichBuilderId model.displayedScenarioBuilderView == Just id

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular
    in
    row [ onMouseEnter (ToggleMenu (Just id))
        , onMouseLeave (ToggleMenu Nothing)
        ]
        [ el [ onRight folderMenuView ] <|
              link [ weight ]
                  { url = href (ScenarioPage (RichRunView id Nothing))
                  , label = el [] <| iconWithTextAndColor "label" name color
                  }
        ]


-- ** folder view


folderView : Uuid -> Model a -> Editable String -> List (Element Msg) -> Bool -> Element Msg
folderView id model eName folderChildrenView open =
    let
        folderWithIconView : Element Msg
        folderWithIconView =
            let
                folderIconText =
                    case open of
                        False ->
                            "keyboard_arrow_right"

                        True ->
                            "keyboard_arrow_down"
            in
            iconWithText folderIconText name

        showMenu =
            Just id == model.displayedScenarioNodeMenuId

        name =
            notEditedValue eName

        selected =
            getRichBuilderId model.displayedScenarioBuilderView == Just id

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular

        folderReadView : Element Msg
        folderReadView =
            Input.button [ weight ]
                { onPress = Just <| ToggleFolder id
                , label = folderWithIconView
                }

        folderMenuView : Element Msg
        folderMenuView =
            case showMenu of
                True ->
                    link []
                        { url = href (ScenarioPage (RichEditView (DefaultEditView id)))
                        , label = editIcon
                        }

                False ->
                    none
    in
    column [ width (fill |> maximum 300) ]
        [ row [ onMouseEnter (ToggleMenu (Just id))
              , onMouseLeave (ToggleMenu Nothing)
              ]
              [ folderReadView
              , folderMenuView
              ]
        , case open of
            True ->
                column [ spacing 10, paddingXY 20 10 ] folderChildrenView

            False ->
                none
        ]
