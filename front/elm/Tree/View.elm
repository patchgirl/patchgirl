module Tree.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tree.Model exposing (Model, Node(..), Tree)
import Tree.Message exposing (Msg(..))
import Tree.FolderView exposing (..)

import Util.View as Util

view : Model -> Html Msg
view model =
  let
    treeView = Tuple.second (nodeView 0 model.displayedNodeMenuIndex model.tree)
  in
    div [ class "columns" ]
      [ div [ class "column is-offset-1" ]
        treeView
      ]

fileReadView : String -> Int -> Html Msg
fileReadView name idx =
  a [ href "#", onClick (SetDisplayedBuilder idx) ]
    [ span [ class "fas fa-file fa-fw" ] []
    , text name
    ]

fileEditView : String -> Int -> Html Msg
fileEditView name idx =
  input [ value name, Util.onEnterWithInput (Rename idx) ] []

nodeView : Int -> Maybe Int -> Tree -> (Int, List (Html Msg))
nodeView idx mDisplayedNodeMenuIndex tree =
  let
    showMenu = mDisplayedNodeMenuIndex == Just idx
  in
    case tree of
      [] -> (idx, [])
      node :: tail ->
        case node of
          (Folder { name, open, children, showRenameInput }) ->
            let
              (folderIdx, folderChildrenView) = nodeView (idx + 1) mDisplayedNodeMenuIndex children
              (newIdx, tailView) = nodeView folderIdx mDisplayedNodeMenuIndex tail
              currentFolderView = folderView name idx folderChildrenView open showMenu showRenameInput
            in
              (newIdx, currentFolderView :: tailView)

          (File { name, showRenameInput }) ->
            let
              modeView =
                case showRenameInput of
                  True -> fileEditView name idx
                  False -> fileReadView name idx
              (newIdx, tailView) = nodeView (idx + 1) mDisplayedNodeMenuIndex tail
              fileView = div [ ]
                           [ modeView
                           , a [ onClick (ToggleMenu idx) ] [ span [] []]
                           , span [ hidden (not showMenu) ]
                             [ a [ class "icono-tag", onClick (ShowRenameInput idx) ] []
                             , a [ class "icono-cross", onClick (Delete idx) ] []
                             ]
                           ]
            in
              (newIdx, fileView :: tailView)
