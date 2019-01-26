module Tree.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Tree.Model exposing (Model, Node(..), Tree)
import Tree.Message exposing (Msg(..))

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

        (File { name }) ->
          let
            (newIdx, tailView) = nodeView (idx + 1) tail
            fileView = li [] [ a [ onClick (SetDisplayedBuilder idx) ] [ text name ]
                             , a [ onClick (ShowRenameInput idx) ] [ text ("rename") ]
                             ]
          in
            (newIdx, fileView :: tailView)
