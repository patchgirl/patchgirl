module BuilderApp.BuilderTree.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

--import BuilderApp.BuilderTree.Model exposing (Model, RequestNode(..), BuilderTree)
import BuilderApp.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (Msg(..))
import BuilderApp.BuilderTree.FolderView exposing (..)
import BuilderApp.BuilderTree.FileView exposing (..)

import Util.View as Util

view : Model a -> List (Element Msg)
view model =
    let
        (RequestCollection _ requestNodes) = model.requestCollection
    in
        Tuple.second (nodeView 0 model.displayedRequestNodeMenuIndex requestNodes)

nodeView : Int -> Maybe Int -> List RequestNode -> (Int, List (Element Msg))
nodeView idx mDisplayedRequestNodeMenuIndex requestCollection =
  let
    showMenu = mDisplayedRequestNodeMenuIndex == Just idx
  in
    case requestCollection of
      [] -> (idx, [])
      node :: tail ->
        case node of
          (RequestFolder { name, open, children }) ->
            let
              (folderIdx, folderChildrenView) = nodeView (idx + 1) mDisplayedRequestNodeMenuIndex children
              (newIdx, tailView) = nodeView folderIdx mDisplayedRequestNodeMenuIndex tail
              currentFolderView =
                  folderView name idx folderChildrenView open showMenu
            in
              (newIdx, currentFolderView :: tailView)

          (RequestFile { name }) ->
            let
              (newIdx, tailView) = nodeView (idx + 1) mDisplayedRequestNodeMenuIndex tail
              currentFileView = fileView name idx showMenu
            in
              (newIdx, [none])--currentFileView :: tailView)
