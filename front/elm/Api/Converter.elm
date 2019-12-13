module Api.Converter exposing(..)

import Api.Generated as Back
import BuilderApp.Model as Front
import Application.Type exposing (..)
import Application.Type as Front

import BuilderApp.Builder.Model as Builder

-- * request Collection

convertRequestCollectionFromBackToFront : Back.RequestCollection -> Front.RequestCollection
convertRequestCollectionFromBackToFront backRequestCollection =
    let
        (Back.RequestCollection id backRequestNodes) = backRequestCollection
    in
        Front.RequestCollection id (convertRequestNodesFromBackToFront backRequestNodes)

convertRequestNodesFromBackToFront : List Back.RequestNode -> List Front.RequestNode
convertRequestNodesFromBackToFront backRequestNodes =
    let
        convertRequestNodeFromBackToFront : Back.RequestNode -> Front.RequestNode
        convertRequestNodeFromBackToFront backRequestNode =
            case backRequestNode of
                Back.RequestFolder folder ->
                    Front.RequestFolder
                        { name = NotEdited folder.name
                        , open = not <| List.isEmpty folder.children
                        , children = convertRequestNodesFromBackToFront folder.children
                        }
                Back.RequestFile file ->
                    Front.RequestFile
                        { name = NotEdited file.name
                        , isSaved = True
                        , httpUrl = NotEdited file.httpUrl
                        , httpMethod = NotEdited file.httpMethod
                        , httpHeaders = NotEdited file.httpHeaders
                        , httpBody = NotEdited file.httpBody
                        , response = Nothing
                        , showResponseView = False
                        }
    in
        List.map convertRequestNodeFromBackToFront backRequestNodes

-- * environment

convertEnvironmentFromBackToFront : Back.Environment -> Front.Environment
convertEnvironmentFromBackToFront { id, name, keyValues } =
    { id = id
    , name = NotEdited name
    , showRenameInput = False
    , keyValues = List.map Saved keyValues
    }

-- * environment key values

convertEnvironmentKeyValueFromBackToFront : Back.KeyValue -> Front.Storable Front.NewKeyValue Front.KeyValue
convertEnvironmentKeyValueFromBackToFront backKeyValue =
    Front.Saved backKeyValue

convertEnvironmentKeyValueFromFrontToBack : Front.Storable Front.NewKeyValue Front.KeyValue -> Back.NewKeyValue
convertEnvironmentKeyValueFromFrontToBack storable =
    case storable of
        New { key, value } ->
            { newKeyValueKey = key
            , newKeyValueValue = value
            }

        Saved { key, value } ->
            { newKeyValueKey = key
            , newKeyValueValue = value
            }

        Edited2 _ { key, value } ->
            { newKeyValueKey = key
            , newKeyValueValue = value
            }

-- * account

convertSessionFromBackToFront : Back.Session -> Front.Session
convertSessionFromBackToFront backSession =
    case backSession of
        Back.VisitorSession { sessionAccountId, sessionCsrfToken } ->
            Front.Visitor { id = sessionAccountId
                          , csrfToken = sessionCsrfToken
                          }

        Back.SignedUserSession { sessionAccountId, sessionCsrfToken, sessionEmail } ->
            Front.SignedUser
                { id = sessionAccountId
                , csrfToken = sessionCsrfToken
                , email = sessionEmail
                }
