module RequestComputation
    exposing ( buildRequestComputationInput
             )

import Application.Type exposing (..)
import Combine
import List.Extra as List
import Regex
import StringTemplate exposing(..)


-- * model


type alias Model a =
    { a
        | httpUrl : Editable String
        , httpMethod : Editable HttpMethod
        , httpHeaders : Editable (List ( String, String ))
        , httpBody : Editable String
    }



-- * build request computation input


buildRequestComputationInput : Model a -> RequestComputationInput
buildRequestComputationInput model =
    { method =
        editedOrNotEditedValue model.httpMethod
    , headers =
        editedOrNotEditedValue model.httpHeaders
            |> List.map (\(h, v) -> (stringToTemplate h, stringToTemplate v))
    , url =
        editedOrNotEditedValue model.httpUrl
            |> cleanUrl
            |> stringToTemplate
    , body =
        editedOrNotEditedValue model.httpBody
            |> stringToTemplate
    }


-- * util


cleanUrl : String -> String
cleanUrl url =
    String.trimLeft url
