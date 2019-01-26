module Tree.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tree.Model exposing (Model, Node(..), Tree)
import Tree.Message exposing (Msg(..))

import Util.View as Util

view : Tree -> Html Msg
view tree =
  ul [] <| Tuple.second (nodeView 0 tree)

nodeView : Int -> Tree -> (Int, List (Html Msg))
nodeView idx tree =
  case tree of
    [] -> (idx, [])
    node :: tail ->
      case node of
        (Folder { name, open, children }) ->
          let
            (folderIdx, folderChildrenView) = nodeView (idx + 1) children
            (newIdx, tailView) = nodeView folderIdx tail
            folderToggleView = if open then "-" else "+"
            folderView =
              li [] [ b [ onClick (ToggleNode idx) ] [ text (folderToggleView ++ " " ++ name ++ "/ ") ]
                    , a [ onClick (Mkdir idx) ] [ text ("new Folder ") ]
                    , a [ onClick (Touch idx) ] [ text ("new File ") ]
                    , a [ onClick (ShowRenameInput idx) ] [ text ("rename") ]
                    , ul [ hidden (not open) ] folderChildrenView
                    ]
          in
            (newIdx, folderView :: tailView)

        (File { name, showRenameInput }) ->
          let
            editView : Html Msg
            editView = input [ value name, Util.onEnterWithInput (Rename idx) ] []
            readView = a [ onClick (SetDisplayedBuilder idx) ] [ text (" " ++ name) ]
            modeView =
              case showRenameInput of
                True -> editView
                False -> readView
            (newIdx, tailView) = nodeView (idx + 1) tail
            fileView = li [] [ modeView
                             , a [ onClick (ShowRenameInput idx) ] [ text ("rename") ]
                             ]
          in
            (newIdx, fileView :: tailView)
