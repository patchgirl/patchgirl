module BuilderApp.BuilderTree.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input

import Uuid
import BuilderApp.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (Msg(..))
import BuilderApp.BuilderTree.FolderView exposing (..)
import BuilderApp.BuilderTree.FileView exposing (..)

import Util.View as Util
import ViewUtil exposing (..)

view : Model a -> Element Msg
view model =
    let
        (RequestCollection _ requestNodes) = model.requestCollection

        mainMenuView =
            row [ spacing 10 ]
                [ Input.button []
                      { onPress = Just <| DoNothing
                      , label = iconWithText "create_new_folder" "new folder"
                      }
                , Input.button []
                      { onPress = Just <| GenerateRandomUUIDForRootFile
                      , label = iconWithText "note_add" "new file"
                      }
                ]

        treeView =
            column [ spacing 10 ] (nodeView model.displayedRequestNodeMenuId requestNodes)

    in
        column [ alignTop, spacing 20, centerX ]
            [ mainMenuView
            , treeView
            ]



nodeView : Maybe Uuid.Uuid -> List RequestNode -> List (Element Msg)
nodeView mDisplayedRequestNodeMenuIndex requestCollection =
    case requestCollection of
      [] -> []
      node :: tail ->
        case node of
          (RequestFolder { id, name, open, children }) ->
            let
              folderChildrenView = nodeView mDisplayedRequestNodeMenuIndex children
              tailView = nodeView mDisplayedRequestNodeMenuIndex tail
              currentFolderView =
                  folderView id mDisplayedRequestNodeMenuIndex name folderChildrenView open
            in
              currentFolderView :: tailView

          (RequestFile { id, name }) ->
            let
              tailView = nodeView mDisplayedRequestNodeMenuIndex tail
              currentFileView = fileView id mDisplayedRequestNodeMenuIndex name
            in
              currentFileView :: tailView
