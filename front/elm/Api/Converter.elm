module Api.Converter exposing(..)

import Api.Client as Back
import BuilderApp.Model as Front
import Application.Type exposing (..)
import Application.Type as Front

import BuilderApp.Builder.Model as Builder

-- * Request Collection

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
                        , httpMethod = file.httpMethod
                        , httpHeaders = NotEdited file.httpHeaders
                        , httpBody = NotEdited file.httpBody
                        , response = Nothing
                        , showResponseView = False
                        }
    in
        List.map convertRequestNodeFromBackToFront backRequestNodes

-- * Environment

convertEnvironmentFromBackToFront : Back.Environment -> Front.Environment
convertEnvironmentFromBackToFront { id, name, keyValues } =
    { id = id
    , name = NotEdited name
    , showRenameInput = False
    , keyValues = NotEdited keyValues
    }
