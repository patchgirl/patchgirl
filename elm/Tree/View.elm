module Tree.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tree.Model exposing (Model, Node(..), Tree)
import Tree.Message exposing (Msg(..))

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
              folderIcon = if open then "fas fa-folder-open fa-fw" else "fas fa-folder fa-fw"
              readView =
                a [ onClick (ToggleNode idx) ]
                  [ span [ class folderIcon ] []
                  , text name
                  ]
              editView = input [ value name, Util.onEnterWithInput (Rename idx) ] []
              modeView =
                case showRenameInput of
                  True -> editView
                  False -> readView
              folderView =
                div []
                  [ span []
                    [ modeView
                    , a [ onClick (ToggleMenu idx) ] [ span [] []]
                    , span [ hidden (not showMenu) ]
                      [ a [ class "icono-tag", onClick (ShowRenameInput idx) ] [ text ("f2 ") ]
                      , a [ class "icono-folder", onClick (Mkdir idx) ] [ text ("+/ ") ]
                      , a [ class "icono-file", onClick (Touch idx) ] [ text ("+. ") ]
                      , a [ class "icono-cross", onClick (Delete idx) ] [ ]
                      ]
                    ]
                  , div [ class "columns" ]
                    [ div [ class "column is-offset-1"]
                      [ div [ hidden (not open) ] folderChildrenView ]
                    ]
                  ]
            in
              (newIdx, folderView :: tailView)

          (File { name, showRenameInput }) ->
            let
              editView = input [ value name, Util.onEnterWithInput (Rename idx) ] []
              readView =
                a [ href "#", onClick (SetDisplayedBuilder idx) ]
                  [ span [ class "fas fa-file fa-fw" ] []
                  , text name
                  ]
              modeView =
                case showRenameInput of
                  True -> editView
                  False -> readView
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
