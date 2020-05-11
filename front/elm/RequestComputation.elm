module RequestComputation
    exposing ( buildRequestComputationInput
             , removeSchemeFromUrl
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
    { scheme =
        editedOrNotEditedValue model.httpUrl
           |> schemeFromUrl
    , method =
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
    removeSchemeFromUrl (String.trimLeft url)


removeSchemeFromUrl : String -> String
removeSchemeFromUrl url =
    let
        schemeRegex : Regex.Regex
        schemeRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromStringWith
                    { caseInsensitive = False
                    , multiline = False
                    }
                    "^https?://"
    in
    Regex.replace schemeRegex (always "") url


schemeFromUrl : String -> Scheme
schemeFromUrl url =
    case String.startsWith "https://" url of
        True ->
            Https

        False ->
            Http
