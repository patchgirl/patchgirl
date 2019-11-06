module BuilderApp.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

import BuilderApp.Message exposing (..)
import BuilderApp.Model exposing (..)

import BuilderApp.BuilderTree.Util as BuilderTree
import Util.List as List
import Util.Maybe as Maybe
import BuilderApp.Builder.View as Builder
import BuilderApp.Builder.Model as Builder
import BuilderApp.BuilderTree.View as BuilderTree

view : Model a -> Element Msg
view model =
    column [ width fill ]
      [ column [ spacing 10, width (px 500) ] <| List.map (map TreeMsg) (BuilderTree.view model)
      , el [] {-builderView model model.selectedBuilderIndex-} none
      ]

{-builderView : Model a -> Maybe Int -> Html Msg
builderView model mIdx =
  let
    (RequestCollection _ requestNodes) = model.requestCollection
    mFile : Int -> Maybe BuilderApp.Model.File
    mFile idx = BuilderTree.findFile requestNodes idx
    title file =
        case file.isSaved of
            True -> file.name
            False -> file.name ++ " *"
  in
    case Maybe.andThen mFile mIdx of
      Just file ->
        div []
            [ h1 [] [ text (title file) ]
            , Html.map BuilderMsg (Builder.view file)
            ]
      Nothing -> div [] []
-}
