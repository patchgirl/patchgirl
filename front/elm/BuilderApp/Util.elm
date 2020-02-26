module BuilderApp.Util exposing (..)

import Application.Type exposing (..)

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

markFileAsSaved : RequestNode -> RequestNode
markFileAsSaved node =
  case node of
      RequestFolder f ->
          RequestFolder f
      RequestFile f ->
          RequestFile { f | isSaved = True }

changeFileBuilder : File -> RequestNode -> RequestNode
changeFileBuilder newFile node =
    case node of
        RequestFolder f ->
            RequestFolder f
        RequestFile f ->
            RequestFile newFile
