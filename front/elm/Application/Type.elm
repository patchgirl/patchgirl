module Application.Type exposing (..)

type alias Environment =
    { id : Int
    , name : String
    , keyValues : Editable (List KeyValue)
    }

type alias KeyValue =
    { id : Int
    , key : String
    , value : String
    }

type Editable a = NotEdited a | Edited a a

isDirty : Editable a -> Bool
isDirty editable =
    case editable of
        NotEdited _ ->
            False

        Edited _ _ ->
            True

editedOrNotEditedValue : Editable a -> a
editedOrNotEditedValue editable =
    case editable of
        NotEdited value ->
            value
        Edited _ newValue ->
            newValue

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

changeEditedValue2 : Editable a -> Editable a -> Editable a
changeEditedValue2 eOldValue eNewValue =
    let
        oldValue = notEditedValue eOldValue
        newValue = editedOrNotEditedValue eNewValue
    in
        case oldValue == newValue of
        True ->
            NotEdited oldValue

        False->
            eNewValue
