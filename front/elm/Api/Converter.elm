module Api.Converter exposing(..)

import Api.Generated as Back
import Application.Type exposing (..)
import Application.Type as Front
import Dict
import Uuid
import Tuple
import Animation


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
                        { id = folder.requestNodeId
                        , name = NotEdited folder.requestNodeName
                        , open = not <| List.isEmpty folder.requestNodeChildren
                        , children = convertRequestNodesFromBackToFront folder.requestNodeChildren
                        }
                Back.RequestFile file ->
                    Front.RequestFile
                        { id = file.requestNodeId
                        , name = NotEdited file.requestNodeName
                        , httpUrl = NotEdited file.requestNodeHttpUrl
                        , httpMethod = NotEdited (convertMethodFromBackToFront file.requestNodeHttpMethod)
                        , httpHeaders = NotEdited file.requestNodeHttpHeaders
                        , httpBody = NotEdited file.requestNodeHttpBody
                        , requestComputationResult = Nothing
                        , showResponseView = False
                        , runRequestIconAnimation = Animation.style []
                        }
    in
        List.map convertRequestNodeFromBackToFront backRequestNodes


-- * scenario collection


convertScenarioCollectionFromBackToFront : Back.ScenarioCollection -> Front.ScenarioCollection
convertScenarioCollectionFromBackToFront backScenarioCollection =
    let
        (Back.ScenarioCollection id backScenarioNodes) = backScenarioCollection
    in
        Front.ScenarioCollection id (convertScenarioNodesFromBackToFront backScenarioNodes)


-- ** scenarioNode


convertScenarioNodesFromBackToFront : List Back.ScenarioNode -> List Front.ScenarioNode
convertScenarioNodesFromBackToFront backScenarioNodes =
    let
        convertScenarioNodeFromBackToFront : Back.ScenarioNode -> Front.ScenarioNode
        convertScenarioNodeFromBackToFront backScenarioNode =
            case backScenarioNode of
                Back.ScenarioFolder folder ->
                    Front.ScenarioFolder
                        { id = folder.scenarioNodeId
                        , name = NotEdited folder.scenarioNodeName
                        , children = convertScenarioNodesFromBackToFront folder.scenarioNodeChildren
                        , open = not (List.isEmpty folder.scenarioNodeChildren)
                        }

                Back.ScenarioFile file ->
                    Front.ScenarioFile
                        { id = file.scenarioNodeId
                        , name = NotEdited file.scenarioNodeName
                        , scenes = List.map convertSceneFromBackToFront file.scenarioNodeScenes
                        }
    in
        List.map convertScenarioNodeFromBackToFront backScenarioNodes


-- ** scene


convertSceneFromBackToFront : Back.Scene -> Front.Scene
convertSceneFromBackToFront { sceneId, sceneRequestFileNodeId } =
    { id = sceneId
    , requestFileNodeId = sceneRequestFileNodeId
    }


-- * environment


convertEnvironmentFromBackToFront : Back.Environment -> Front.Environment
convertEnvironmentFromBackToFront { environmentId, environmentName, environmentKeyValues } =
    { id = environmentId
    , name = NotEdited environmentName
    , showRenameInput = False
    , keyValues = List.map convertEnvironmentKeyValueFromBackToFront environmentKeyValues
    }


-- * environment key values


convertEnvironmentKeyValueFromBackToFront : Back.KeyValue -> Front.Storable Front.NewKeyValue Front.KeyValue
convertEnvironmentKeyValueFromBackToFront { keyValueId, keyValueKey, keyValueValue } =
    Saved { id = keyValueId
          , key = keyValueKey
          , value = keyValueValue
          }

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
                          , signInEmail = ""
                          , signInPassword = ""
                          , signInErrors = []
                          , signUpEmail = ""
                          , signUpError = Nothing
                          , signUpMessage = Nothing
                          }

        Back.SignedUserSession { sessionAccountId, sessionCsrfToken, sessionGithubEmail, sessionGithubAvatarUrl } ->
            Front.SignedUser
                { id = sessionAccountId
                , csrfToken = sessionCsrfToken
                , email = sessionGithubEmail
                , avatarUrl = sessionGithubAvatarUrl
                }


-- * request computation


convertRequestComputationResultFromBackToFront : Back.RequestComputationResult -> Front.RequestComputationResult
convertRequestComputationResultFromBackToFront backRequestComputationResult =
    case backRequestComputationResult of
        Back.RequestComputationSucceeded { requestComputationOutputStatusCode
                                         , requestComputationOutputHeaders
                                         , requestComputationOutputBody
                                         } ->
            Front.RequestComputationSucceeded
                { statusCode = requestComputationOutputStatusCode
                , statusText = ""
                , headers = Dict.fromList <| List.map (Tuple.mapFirst String.toLower) requestComputationOutputHeaders
                , body = requestComputationOutputBody
                }

        Back.RequestComputationFailed httpException ->
            let
                frontException = case httpException of
                    Back.InvalidUrlException a b -> Front.InvalidUrlException a b
                    Back.TooManyRedirects -> Front.TooManyRedirects
                    Back.OverlongHeaders -> Front.OverlongHeaders
                    Back.ResponseTimeout -> Front.ResponseTimeout
                    Back.ConnectionTimeout -> Front.ConnectionTimeout
                    Back.ConnectionFailure a -> Front.ConnectionFailure a
                    Back.InvalidStatusLine -> Front.InvalidStatusLine
                    Back.InvalidHeader -> Front.InvalidHeader
                    Back.InvalidRequestHeader -> Front.InvalidRequestHeader
                    Back.InternalException -> Front.InternalException
                    Back.ProxyConnectException -> Front.ProxyConnectException
                    Back.NoResponseDataReceived -> Front.NoResponseDataReceived
                    Back.WrongRequestBodyStreamSize -> Front.WrongRequestBodyStreamSize
                    Back.ResponseBodyTooShort -> Front.ResponseBodyTooShort
                    Back.InvalidChunkHeaders -> Front.InvalidChunkHeaders
                    Back.IncompleteHeaders -> Front.IncompleteHeaders
                    Back.InvalidDestinationHost -> Front.InvalidDestinationHost
                    Back.HttpZlibException -> Front.HttpZlibException
                    Back.InvalidProxyEnvironmentVariable -> Front.InvalidProxyEnvironmentVariable
                    Back.ConnectionClosed -> Front.ConnectionClosed
                    Back.InvalidProxySettings -> Front.InvalidProxySettings
                    Back.UnknownException -> Front.UnknownException

            in
                RequestComputationFailed frontException

convertRequestComputationInputFromFrontToBack : Front.RequestComputationInput -> Back.RequestComputationInput
convertRequestComputationInputFromFrontToBack frontRequestInput =
   { requestComputationInputMethod = convertMethodFromFrontToBack frontRequestInput.method
   , requestComputationInputHeaders = frontRequestInput.headers
   , requestComputationInputScheme = convertSchemeFromFrontToBack frontRequestInput.scheme
   , requestComputationInputUrl = frontRequestInput.url
   , requestComputationInputBody = frontRequestInput.body
   }

convertMethodFromBackToFront : Back.Method -> Front.HttpMethod
convertMethodFromBackToFront method =
    case method of
        Back.Get -> Front.HttpGet
        Back.Post -> Front.HttpPost
        Back.Put -> Front.HttpPut
        Back.Delete -> Front.HttpDelete
        Back.Patch -> Front.HttpPatch
        Back.Head -> Front.HttpHead
        Back.Options -> Front.HttpOptions

convertMethodFromFrontToBack : Front.HttpMethod -> Back.Method
convertMethodFromFrontToBack method =
    case method of
        Front.HttpGet -> Back.Get
        Front.HttpPost -> Back.Post
        Front.HttpPut -> Back.Put
        Front.HttpDelete -> Back.Delete
        Front.HttpPatch -> Back.Patch
        Front.HttpHead -> Back.Head
        Front.HttpOptions -> Back.Options

convertSchemeFromFrontToBack : Front.Scheme -> Back.Scheme
convertSchemeFromFrontToBack scheme =
    case scheme of
        Front.Http -> Back.Http
        Front.Https -> Back.Https


-- * scenario computation


convertScenarioComputationOutputFromBackToFront : Back.ScenarioComputationOutput -> Front.ScenarioComputationOutput
convertScenarioComputationOutputFromBackToFront backScenarioComputationOutput =
    let
        convertOutputSceneFromBackToFront : Back.OutputScene -> Front.OutputScene
        convertOutputSceneFromBackToFront backOutputScene =
            { sceneId = backOutputScene.outputSceneId
            , requestFileNodeId = backOutputScene.outputSceneRequestFileNodeId
            , requestComputationOutput = convertSceneComputationFromBackToFront backOutputScene.outputSceneRequestComputationOutput
            }

        convertSceneComputationFromBackToFront : Back.SceneComputation -> Front.SceneComputation
        convertSceneComputationFromBackToFront backSceneComputation =
            case backSceneComputation of
                Back.SceneRun requestComputationResult ->
                    Front.SceneRun (convertRequestComputationResultFromBackToFront requestComputationResult)
                Back.SceneNotRun ->
                    Front.SceneNotRun

    in
        { scenarioId = backScenarioComputationOutput.outputScenarioId
        , scenes = List.map convertOutputSceneFromBackToFront backScenarioComputationOutput.outputScenarioScenes
        }
