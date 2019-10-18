module BuilderApp.Util exposing (..)

import BuilderApp.Builder.Model as Builder
import BuilderApp.Model exposing (..)

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

markFileAsSaved : Node -> Node
markFileAsSaved node =
  case node of
      RequestFolder f ->
          RequestFolder f
      RequestFile f ->
          RequestFile { f | isSaved = True }

changeFileBuilder : Builder.Model -> Node -> Node
changeFileBuilder newBuilder node =
    case node of
        RequestFolder f ->
            RequestFolder f
        RequestFile f ->
            RequestFile { f | builder = newBuilder }
