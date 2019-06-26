module BuilderApp.Util exposing (..)

import BuilderApp.BuilderTree.Model as BuilderTree
import BuilderApp.Builder.Model as Builder

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

markFileAsSaved : BuilderTree.Node -> BuilderTree.Node
markFileAsSaved node =
  case node of
    BuilderTree.Folder f -> BuilderTree.Folder f
    BuilderTree.File f -> BuilderTree.File { f | isSaved = True }

changeFileBuilder : Builder.Model -> BuilderTree.Node -> BuilderTree.Node
changeFileBuilder newBuilder node =
  case node of
    BuilderTree.Folder f -> BuilderTree.Folder f
    BuilderTree.File f -> BuilderTree.File { f | builder = newBuilder }
