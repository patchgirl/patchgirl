module RequestComputation exposing (buildRequestComputationInput)

import Application.Type exposing (..)
import Regex
import List.Extra as List
import Combine


-- * model


type alias Model a =
    { a | httpUrl : Editable String
    , httpMethod : Editable HttpMethod
    , httpHeaders : Editable (List (String, String))
    , httpBody : Editable String
    }


-- * build request computation input


buildRequestComputationInput : List (Storable NewKeyValue KeyValue) -> Model a -> RequestComputationInput
buildRequestComputationInput envKeyValues builder =
    { scheme =
          schemeFromUrl (interpolate envKeyValues (editedOrNotEditedValue builder.httpUrl))
    , method =
        editedOrNotEditedValue builder.httpMethod
    , headers =
        List.map (interpolateHeader envKeyValues) (editedOrNotEditedValue builder.httpHeaders)
    , url =
        cleanUrl <| interpolate envKeyValues (editedOrNotEditedValue builder.httpUrl)
    , body =
        interpolate envKeyValues (editedOrNotEditedValue builder.httpBody)
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
                Regex.fromStringWith { caseInsensitive = False
                                     , multiline = False
                                     } "^https?://"
    in
        Regex.replace schemeRegex (\_ -> "") url

schemeFromUrl : String -> Scheme
schemeFromUrl url =
    case String.startsWith "https://" url of
        True ->
            Https

        False ->
            Http

interpolateHeader : List (Storable NewKeyValue KeyValue) -> (String, String) -> (String, String)
interpolateHeader envKeyValues (headerKey, headerValue) =
    ( interpolate envKeyValues headerKey
    , interpolate envKeyValues headerValue
    )


interpolate : List (Storable NewKeyValue KeyValue) -> String -> String
interpolate envKeys str =
  let
    build : TemplatedString -> String
    build templatedString =
      case templatedString of
        Sentence c -> c
        Key k ->
          case List.find (\sEnvKey ->
                              case sEnvKey of
                                  New { key } ->
                                      key == k

                                  Saved { key } ->
                                      key == k

                                  Edited2 _ { key } ->
                                      key == k
                         ) envKeys of
            Just sEnvKey ->
                case sEnvKey of
                    New { value } ->
                        value

                    Saved { value } ->
                        value

                    Edited2 _ { value } ->
                        value
            Nothing ->
                "{{" ++ k ++ "}}"
  in
    case toRaw str of
      Ok templatedStrings -> List.map build templatedStrings |> List.foldr (++) ""
      Err errors -> Debug.log errors str


-- * parser


{-
 parse a string "hello {{user}} !" to a list of templated string
 eg: [ Sentence "hello "
     , Key "user"
     , Sentence " !"
     ]
-}
toRaw : String -> Result String (List TemplatedString)
toRaw str =
  case Combine.parse templatedStringParser str of
    Ok (_, _, result) ->
      Ok result

    Err (_, stream, errors) ->
      Err (String.join " or " errors)

type TemplatedString
    = Sentence String
    | Key String

templatedStringParser : Combine.Parser s (List TemplatedString)
templatedStringParser = Combine.many (Combine.or keyParser anychar)

anychar : Combine.Parser s TemplatedString
anychar = Combine.regex ".\n*" |> Combine.map Sentence

keyParser : Combine.Parser s TemplatedString
keyParser =
  let
    envKey : Combine.Parser s TemplatedString
    envKey = Combine.regex "([a-zA-Z]|[0-9]|-)+" |> Combine.map Key
  in
    Combine.between (Combine.string "{{") (Combine.string "}}") envKey
