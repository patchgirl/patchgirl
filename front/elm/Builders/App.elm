module Builders.App exposing (..)

import Builders.Model exposing (..)
import Builders.Message exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab idx ->
      ( { model | selectedBuilderIndex = Just idx }, Cmd.none)

    CloseTab idx ->
      let
        findPrevious : List a -> a -> Maybe a
        findPrevious l a =
          case l of
            x :: y :: z ->
              if a == x then
                Just y
              else if a == y then
                Just x
              else
                findPrevious (y :: z) a
            x :: xs -> findPrevious xs a
            [] -> Nothing

        newDisplayedBuilderIndexes = List.filter (\x -> x /= idx) model.displayedBuilderIndexes
        newSelectedBuilderIndex =
          case Just idx == model.selectedBuilderIndex of
            False -> model.selectedBuilderIndex
            True -> findPrevious model.displayedBuilderIndexes idx
      in
        ( { model | displayedBuilderIndexes = newDisplayedBuilderIndexes,
          selectedBuilderIndex = newSelectedBuilderIndex }, Cmd.none)

    _ ->
      (model, Cmd.none)
