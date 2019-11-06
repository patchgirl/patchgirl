module Application.Type exposing (..)

type alias Environment =
    { name : String
    , keyValues : List(String, String)
    }

type Editable a = NotEdited a | Edited a a

notEditedValue : Editable a -> a
notEditedValue editable =
    case editable of
        NotEdited value ->
            value
        Edited value _ ->
            value

editedValue : Editable a -> Maybe a
editedValue editable =
    case editable of
        NotEdited _ -> Nothing
        Edited _ value -> Just value

changeEditedValue : a -> Editable a -> Editable a
changeEditedValue newValue editable =
    let
        oldValue = notEditedValue editable
    in
        Edited oldValue newValue
