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
        (Folder { name, open, children, showRenameInput }) ->
          let
            (folderIdx, folderChildrenView) = nodeView (idx + 1) children
            (newIdx, tailView) = nodeView folderIdx tail
            folderToggleView = if open then "-" else "+"
            readView = b [ onClick (ToggleNode idx) ] [ text (folderToggleView ++ " " ++ name ++ "/ ") ]
            editView = input [ value name, Util.onEnterWithInput (Rename idx) ] []
            modeView =
              case showRenameInput of
                True -> editView
                False -> readView
            folderView =
              li [] [ modeView
                    , a [ onClick (Mkdir idx) ] [ text ("+/ ") ]
                    , a [ onClick (Touch idx) ] [ text ("+. ") ]
                    , a [ onClick (ShowRenameInput idx) ] [ text ("f2 ") ]
                    , a [ onClick (Delete idx) ] [ text ("- ") ]
                    , ul [ hidden (not open) ] folderChildrenView
                    ]
          in
            (newIdx, folderView :: tailView)

        (File { name, showRenameInput }) ->
          let
            editView = input [ value name, Util.onEnterWithInput (Rename idx) ] []
            readView = a [ href "#", onClick (SetDisplayedBuilder idx) ] [ text (" " ++ name) ]
            modeView =
              case showRenameInput of
                True -> editView
                False -> readView
            (newIdx, tailView) = nodeView (idx + 1) tail
            fileView = li [] [ modeView
                             , a [ onClick (ShowRenameInput idx) ] [ text ("f2 ") ]
                             , a [ onClick (Delete idx) ] [ text ("- ") ]
                             ]
          in
            (newIdx, fileView :: tailView)
