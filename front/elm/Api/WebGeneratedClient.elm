module Api.WebGeneratedClient exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder
import Uuid as Uuid

type alias UUID = Uuid.Uuid

jsonDecUUID : Json.Decode.Decoder UUID
jsonDecUUID = Uuid.decoder

jsonEncUUID : UUID -> Value
jsonEncUUID = Uuid.encode

type alias Either a b = Result a b

jsonDecEither : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Either a b)
jsonDecEither decoder1 decoder2 =
  Json.Decode.oneOf [ Json.Decode.field "Left" decoder1 |> Json.Decode.map Err
                    , Json.Decode.field "Right" decoder2 |> Json.Decode.map Ok
                    ]

jsonEncEither : (a -> Value) -> (b -> Value) -> Either a b -> Value
jsonEncEither encoder1 encoder2 either =
  case either of
    Err err -> encoder1 err
    Ok ok -> encoder2 ok


type NoContent  =
    NoContent 

jsonDecNoContent : Json.Decode.Decoder ( NoContent )
jsonDecNoContent = 
    let jsonDecDictNoContent = Dict.fromList [("NoContent", NoContent)]
    in  decodeSumUnaries "NoContent" jsonDecDictNoContent

jsonEncNoContent : NoContent -> Value
jsonEncNoContent  val =
    case val of
        NoContent -> Json.Encode.string "NoContent"



type alias Token  = String

jsonDecToken : Json.Decode.Decoder ( Token )
jsonDecToken =
    Json.Decode.string

jsonEncToken : Token -> Value
jsonEncToken  val = Json.Encode.string val



type RequestCollection  =
    RequestCollection Int (List RequestNode)

jsonDecRequestCollection : Json.Decode.Decoder ( RequestCollection )
jsonDecRequestCollection =
    Json.Decode.lazy (\_ -> Json.Decode.map2 RequestCollection (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.list (jsonDecRequestNode))))


jsonEncRequestCollection : RequestCollection -> Value
jsonEncRequestCollection (RequestCollection v1 v2) =
    Json.Encode.list identity [Json.Encode.int v1, (Json.Encode.list jsonEncRequestNode) v2]



type RequestNode  =
    RequestFolder {requestNodeId: UUID, requestNodeName: String, requestNodeChildren: (List RequestNode)}
    | RequestFile {requestNodeId: UUID, requestNodeName: String, requestNodeHttpUrl: String, requestNodeHttpMethod: Method, requestNodeHttpHeaders: (List (String, String)), requestNodeHttpBody: String}

jsonDecRequestNode : Json.Decode.Decoder ( RequestNode )
jsonDecRequestNode =
    let jsonDecDictRequestNode = Dict.fromList
            [ ("RequestFolder", Json.Decode.lazy (\_ -> Json.Decode.map RequestFolder (   Json.Decode.succeed (\prequestNodeId prequestNodeName prequestNodeChildren -> {requestNodeId = prequestNodeId, requestNodeName = prequestNodeName, requestNodeChildren = prequestNodeChildren})    |> required "requestNodeId" (jsonDecUUID)    |> required "requestNodeName" (Json.Decode.string)    |> required "requestNodeChildren" (Json.Decode.list (jsonDecRequestNode)))))
            , ("RequestFile", Json.Decode.lazy (\_ -> Json.Decode.map RequestFile (   Json.Decode.succeed (\prequestNodeId prequestNodeName prequestNodeHttpUrl prequestNodeHttpMethod prequestNodeHttpHeaders prequestNodeHttpBody -> {requestNodeId = prequestNodeId, requestNodeName = prequestNodeName, requestNodeHttpUrl = prequestNodeHttpUrl, requestNodeHttpMethod = prequestNodeHttpMethod, requestNodeHttpHeaders = prequestNodeHttpHeaders, requestNodeHttpBody = prequestNodeHttpBody})    |> required "requestNodeId" (jsonDecUUID)    |> required "requestNodeName" (Json.Decode.string)    |> required "requestNodeHttpUrl" (Json.Decode.string)    |> required "requestNodeHttpMethod" (jsonDecMethod)    |> required "requestNodeHttpHeaders" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.string))))    |> required "requestNodeHttpBody" (Json.Decode.string))))
            ]
        jsonDecObjectSetRequestNode = Set.fromList ["RequestFolder", "RequestFile"]
    in  decodeSumTaggedObject "RequestNode" "tag" "contents" jsonDecDictRequestNode jsonDecObjectSetRequestNode

jsonEncRequestNode : RequestNode -> Value
jsonEncRequestNode  val =
    let keyval v = case v of
                    RequestFolder vs -> ("RequestFolder", encodeObject [("requestNodeId", jsonEncUUID vs.requestNodeId), ("requestNodeName", Json.Encode.string vs.requestNodeName), ("requestNodeChildren", (Json.Encode.list jsonEncRequestNode) vs.requestNodeChildren)])
                    RequestFile vs -> ("RequestFile", encodeObject [("requestNodeId", jsonEncUUID vs.requestNodeId), ("requestNodeName", Json.Encode.string vs.requestNodeName), ("requestNodeHttpUrl", Json.Encode.string vs.requestNodeHttpUrl), ("requestNodeHttpMethod", jsonEncMethod vs.requestNodeHttpMethod), ("requestNodeHttpHeaders", (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(Json.Encode.string) t1,(Json.Encode.string) t2])) vs.requestNodeHttpHeaders), ("requestNodeHttpBody", Json.Encode.string vs.requestNodeHttpBody)])
    in encodeSumTaggedObject "tag" "contents" keyval val



type Method  =
    Get 
    | Post 
    | Put 
    | Delete 
    | Patch 
    | Head 
    | Options 

jsonDecMethod : Json.Decode.Decoder ( Method )
jsonDecMethod = 
    let jsonDecDictMethod = Dict.fromList [("Get", Get), ("Post", Post), ("Put", Put), ("Delete", Delete), ("Patch", Patch), ("Head", Head), ("Options", Options)]
    in  decodeSumUnaries "Method" jsonDecDictMethod

jsonEncMethod : Method -> Value
jsonEncMethod  val =
    case val of
        Get -> Json.Encode.string "Get"
        Post -> Json.Encode.string "Post"
        Put -> Json.Encode.string "Put"
        Delete -> Json.Encode.string "Delete"
        Patch -> Json.Encode.string "Patch"
        Head -> Json.Encode.string "Head"
        Options -> Json.Encode.string "Options"



type alias AppHealth  =
   { sAppRunning: Bool
   , sDBUp: Bool
   }

jsonDecAppHealth : Json.Decode.Decoder ( AppHealth )
jsonDecAppHealth =
   Json.Decode.succeed (\psAppRunning psDBUp -> {sAppRunning = psAppRunning, sDBUp = psDBUp})
   |> required "sAppRunning" (Json.Decode.bool)
   |> required "sDBUp" (Json.Decode.bool)

jsonEncAppHealth : AppHealth -> Value
jsonEncAppHealth  val =
   Json.Encode.object
   [ ("sAppRunning", Json.Encode.bool val.sAppRunning)
   , ("sDBUp", Json.Encode.bool val.sDBUp)
   ]



type alias NewRequestFile  =
   { newRequestFileId: UUID
   , newRequestFileParentNodeId: UUID
   }

jsonDecNewRequestFile : Json.Decode.Decoder ( NewRequestFile )
jsonDecNewRequestFile =
   Json.Decode.succeed (\pnewRequestFileId pnewRequestFileParentNodeId -> {newRequestFileId = pnewRequestFileId, newRequestFileParentNodeId = pnewRequestFileParentNodeId})
   |> required "newRequestFileId" (jsonDecUUID)
   |> required "newRequestFileParentNodeId" (jsonDecUUID)

jsonEncNewRequestFile : NewRequestFile -> Value
jsonEncNewRequestFile  val =
   Json.Encode.object
   [ ("newRequestFileId", jsonEncUUID val.newRequestFileId)
   , ("newRequestFileParentNodeId", jsonEncUUID val.newRequestFileParentNodeId)
   ]



type ParentNodeId  =
    RequestCollectionId Int
    | RequestNodeId UUID

jsonDecParentNodeId : Json.Decode.Decoder ( ParentNodeId )
jsonDecParentNodeId =
    let jsonDecDictParentNodeId = Dict.fromList
            [ ("RequestCollectionId", Json.Decode.lazy (\_ -> Json.Decode.map RequestCollectionId (Json.Decode.int)))
            , ("RequestNodeId", Json.Decode.lazy (\_ -> Json.Decode.map RequestNodeId (jsonDecUUID)))
            ]
        jsonDecObjectSetParentNodeId = Set.fromList []
    in  decodeSumTaggedObject "ParentNodeId" "tag" "contents" jsonDecDictParentNodeId jsonDecObjectSetParentNodeId

jsonEncParentNodeId : ParentNodeId -> Value
jsonEncParentNodeId  val =
    let keyval v = case v of
                    RequestCollectionId v1 -> ("RequestCollectionId", encodeValue (Json.Encode.int v1))
                    RequestNodeId v1 -> ("RequestNodeId", encodeValue (jsonEncUUID v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type UpdateRequestNode  = UpdateRequestNode
   { updateRequestNodeName: String
   }

jsonDecUpdateRequestNode : Json.Decode.Decoder ( UpdateRequestNode )
jsonDecUpdateRequestNode =
   Json.Decode.succeed (\pupdateRequestNodeName -> (UpdateRequestNode {updateRequestNodeName = pupdateRequestNodeName}))
   |> required "updateRequestNodeName" (Json.Decode.string)

jsonEncUpdateRequestNode : UpdateRequestNode -> Value
jsonEncUpdateRequestNode  (UpdateRequestNode val) =
   Json.Encode.object
   [ ("updateRequestNodeName", Json.Encode.string val.updateRequestNodeName)
   ]



type NewEnvironment  = NewEnvironment
   { newEnvironmentName: String
   }

jsonDecNewEnvironment : Json.Decode.Decoder ( NewEnvironment )
jsonDecNewEnvironment =
   Json.Decode.succeed (\pnewEnvironmentName -> (NewEnvironment {newEnvironmentName = pnewEnvironmentName}))
   |> required "newEnvironmentName" (Json.Decode.string)

jsonEncNewEnvironment : NewEnvironment -> Value
jsonEncNewEnvironment  (NewEnvironment val) =
   Json.Encode.object
   [ ("newEnvironmentName", Json.Encode.string val.newEnvironmentName)
   ]



type UpdateEnvironment  = UpdateEnvironment
   { updateEnvironmentName: String
   }

jsonDecUpdateEnvironment : Json.Decode.Decoder ( UpdateEnvironment )
jsonDecUpdateEnvironment =
   Json.Decode.succeed (\pupdateEnvironmentName -> (UpdateEnvironment {updateEnvironmentName = pupdateEnvironmentName}))
   |> required "updateEnvironmentName" (Json.Decode.string)

jsonEncUpdateEnvironment : UpdateEnvironment -> Value
jsonEncUpdateEnvironment  (UpdateEnvironment val) =
   Json.Encode.object
   [ ("updateEnvironmentName", Json.Encode.string val.updateEnvironmentName)
   ]



type alias Environment  =
   { environmentId: Int
   , environmentName: String
   , environmentKeyValues: (List KeyValue)
   }

jsonDecEnvironment : Json.Decode.Decoder ( Environment )
jsonDecEnvironment =
   Json.Decode.succeed (\penvironmentId penvironmentName penvironmentKeyValues -> {environmentId = penvironmentId, environmentName = penvironmentName, environmentKeyValues = penvironmentKeyValues})
   |> required "environmentId" (Json.Decode.int)
   |> required "environmentName" (Json.Decode.string)
   |> required "environmentKeyValues" (Json.Decode.list (jsonDecKeyValue))

jsonEncEnvironment : Environment -> Value
jsonEncEnvironment  val =
   Json.Encode.object
   [ ("environmentId", Json.Encode.int val.environmentId)
   , ("environmentName", Json.Encode.string val.environmentName)
   , ("environmentKeyValues", (Json.Encode.list jsonEncKeyValue) val.environmentKeyValues)
   ]



type alias KeyValue  =
   { keyValueId: Int
   , keyValueKey: String
   , keyValueValue: String
   }

jsonDecKeyValue : Json.Decode.Decoder ( KeyValue )
jsonDecKeyValue =
   Json.Decode.succeed (\pkeyValueId pkeyValueKey pkeyValueValue -> {keyValueId = pkeyValueId, keyValueKey = pkeyValueKey, keyValueValue = pkeyValueValue})
   |> required "keyValueId" (Json.Decode.int)
   |> required "keyValueKey" (Json.Decode.string)
   |> required "keyValueValue" (Json.Decode.string)

jsonEncKeyValue : KeyValue -> Value
jsonEncKeyValue  val =
   Json.Encode.object
   [ ("keyValueId", Json.Encode.int val.keyValueId)
   , ("keyValueKey", Json.Encode.string val.keyValueKey)
   , ("keyValueValue", Json.Encode.string val.keyValueValue)
   ]



type alias NewKeyValue  =
   { newKeyValueKey: String
   , newKeyValueValue: String
   }

jsonDecNewKeyValue : Json.Decode.Decoder ( NewKeyValue )
jsonDecNewKeyValue =
   Json.Decode.succeed (\pnewKeyValueKey pnewKeyValueValue -> {newKeyValueKey = pnewKeyValueKey, newKeyValueValue = pnewKeyValueValue})
   |> required "newKeyValueKey" (Json.Decode.string)
   |> required "newKeyValueValue" (Json.Decode.string)

jsonEncNewKeyValue : NewKeyValue -> Value
jsonEncNewKeyValue  val =
   Json.Encode.object
   [ ("newKeyValueKey", Json.Encode.string val.newKeyValueKey)
   , ("newKeyValueValue", Json.Encode.string val.newKeyValueValue)
   ]



type alias CaseInsensitive  = String

jsonDecCaseInsensitive : Json.Decode.Decoder ( CaseInsensitive )
jsonDecCaseInsensitive =
    Json.Decode.string

jsonEncCaseInsensitive : CaseInsensitive -> Value
jsonEncCaseInsensitive  val = Json.Encode.string val



type Session  =
    VisitorSession {sessionAccountId: UUID, sessionCsrfToken: String}
    | SignedUserSession {sessionAccountId: UUID, sessionCsrfToken: String, sessionGithubEmail: String, sessionGithubAvatarUrl: String}

jsonDecSession : Json.Decode.Decoder ( Session )
jsonDecSession =
    let jsonDecDictSession = Dict.fromList
            [ ("VisitorSession", Json.Decode.lazy (\_ -> Json.Decode.map VisitorSession (   Json.Decode.succeed (\psessionAccountId psessionCsrfToken -> {sessionAccountId = psessionAccountId, sessionCsrfToken = psessionCsrfToken})    |> required "sessionAccountId" (jsonDecUUID)    |> required "sessionCsrfToken" (Json.Decode.string))))
            , ("SignedUserSession", Json.Decode.lazy (\_ -> Json.Decode.map SignedUserSession (   Json.Decode.succeed (\psessionAccountId psessionCsrfToken psessionGithubEmail psessionGithubAvatarUrl -> {sessionAccountId = psessionAccountId, sessionCsrfToken = psessionCsrfToken, sessionGithubEmail = psessionGithubEmail, sessionGithubAvatarUrl = psessionGithubAvatarUrl})    |> required "sessionAccountId" (jsonDecUUID)    |> required "sessionCsrfToken" (Json.Decode.string)    |> required "sessionGithubEmail" (Json.Decode.string)    |> required "sessionGithubAvatarUrl" (Json.Decode.string))))
            ]
        jsonDecObjectSetSession = Set.fromList ["VisitorSession", "SignedUserSession"]
    in  decodeSumTaggedObject "Session" "tag" "contents" jsonDecDictSession jsonDecObjectSetSession

jsonEncSession : Session -> Value
jsonEncSession  val =
    let keyval v = case v of
                    VisitorSession vs -> ("VisitorSession", encodeObject [("sessionAccountId", jsonEncUUID vs.sessionAccountId), ("sessionCsrfToken", Json.Encode.string vs.sessionCsrfToken)])
                    SignedUserSession vs -> ("SignedUserSession", encodeObject [("sessionAccountId", jsonEncUUID vs.sessionAccountId), ("sessionCsrfToken", Json.Encode.string vs.sessionCsrfToken), ("sessionGithubEmail", Json.Encode.string vs.sessionGithubEmail), ("sessionGithubAvatarUrl", Json.Encode.string vs.sessionGithubAvatarUrl)])
    in encodeSumTaggedObject "tag" "contents" keyval val



type Scheme  =
    Http 
    | Https 

jsonDecScheme : Json.Decode.Decoder ( Scheme )
jsonDecScheme = 
    let jsonDecDictScheme = Dict.fromList [("Http", Http), ("Https", Https)]
    in  decodeSumUnaries "Scheme" jsonDecDictScheme

jsonEncScheme : Scheme -> Value
jsonEncScheme  val =
    case val of
        Http -> Json.Encode.string "Http"
        Https -> Json.Encode.string "Https"



type alias NewRequestFolder  =
   { newRequestFolderId: UUID
   , newRequestFolderParentNodeId: UUID
   , newRequestFolderName: String
   }

jsonDecNewRequestFolder : Json.Decode.Decoder ( NewRequestFolder )
jsonDecNewRequestFolder =
   Json.Decode.succeed (\pnewRequestFolderId pnewRequestFolderParentNodeId pnewRequestFolderName -> {newRequestFolderId = pnewRequestFolderId, newRequestFolderParentNodeId = pnewRequestFolderParentNodeId, newRequestFolderName = pnewRequestFolderName})
   |> required "newRequestFolderId" (jsonDecUUID)
   |> required "newRequestFolderParentNodeId" (jsonDecUUID)
   |> required "newRequestFolderName" (Json.Decode.string)

jsonEncNewRequestFolder : NewRequestFolder -> Value
jsonEncNewRequestFolder  val =
   Json.Encode.object
   [ ("newRequestFolderId", jsonEncUUID val.newRequestFolderId)
   , ("newRequestFolderParentNodeId", jsonEncUUID val.newRequestFolderParentNodeId)
   , ("newRequestFolderName", Json.Encode.string val.newRequestFolderName)
   ]



type NewRootRequestFile  = NewRootRequestFile
   { newRootRequestFileId: UUID
   }

jsonDecNewRootRequestFile : Json.Decode.Decoder ( NewRootRequestFile )
jsonDecNewRootRequestFile =
   Json.Decode.succeed (\pnewRootRequestFileId -> (NewRootRequestFile {newRootRequestFileId = pnewRootRequestFileId}))
   |> required "newRootRequestFileId" (jsonDecUUID)

jsonEncNewRootRequestFile : NewRootRequestFile -> Value
jsonEncNewRootRequestFile  (NewRootRequestFile val) =
   Json.Encode.object
   [ ("newRootRequestFileId", jsonEncUUID val.newRootRequestFileId)
   ]



type NewRootRequestFolder  = NewRootRequestFolder
   { newRootRequestFolderId: UUID
   }

jsonDecNewRootRequestFolder : Json.Decode.Decoder ( NewRootRequestFolder )
jsonDecNewRootRequestFolder =
   Json.Decode.succeed (\pnewRootRequestFolderId -> (NewRootRequestFolder {newRootRequestFolderId = pnewRootRequestFolderId}))
   |> required "newRootRequestFolderId" (jsonDecUUID)

jsonEncNewRootRequestFolder : NewRootRequestFolder -> Value
jsonEncNewRootRequestFolder  (NewRootRequestFolder val) =
   Json.Encode.object
   [ ("newRootRequestFolderId", jsonEncUUID val.newRootRequestFolderId)
   ]



type alias UpdateRequestFile  =
   { updateRequestFileName: String
   , updateRequestFileHttpUrl: String
   , updateRequestFileHttpMethod: Method
   , updateRequestFileHttpHeaders: (List HttpHeader)
   , updateRequestFileHttpBody: String
   }

jsonDecUpdateRequestFile : Json.Decode.Decoder ( UpdateRequestFile )
jsonDecUpdateRequestFile =
   Json.Decode.succeed (\pupdateRequestFileName pupdateRequestFileHttpUrl pupdateRequestFileHttpMethod pupdateRequestFileHttpHeaders pupdateRequestFileHttpBody -> {updateRequestFileName = pupdateRequestFileName, updateRequestFileHttpUrl = pupdateRequestFileHttpUrl, updateRequestFileHttpMethod = pupdateRequestFileHttpMethod, updateRequestFileHttpHeaders = pupdateRequestFileHttpHeaders, updateRequestFileHttpBody = pupdateRequestFileHttpBody})
   |> required "updateRequestFileName" (Json.Decode.string)
   |> required "updateRequestFileHttpUrl" (Json.Decode.string)
   |> required "updateRequestFileHttpMethod" (jsonDecMethod)
   |> required "updateRequestFileHttpHeaders" (Json.Decode.list (jsonDecHttpHeader))
   |> required "updateRequestFileHttpBody" (Json.Decode.string)

jsonEncUpdateRequestFile : UpdateRequestFile -> Value
jsonEncUpdateRequestFile  val =
   Json.Encode.object
   [ ("updateRequestFileName", Json.Encode.string val.updateRequestFileName)
   , ("updateRequestFileHttpUrl", Json.Encode.string val.updateRequestFileHttpUrl)
   , ("updateRequestFileHttpMethod", jsonEncMethod val.updateRequestFileHttpMethod)
   , ("updateRequestFileHttpHeaders", (Json.Encode.list jsonEncHttpHeader) val.updateRequestFileHttpHeaders)
   , ("updateRequestFileHttpBody", Json.Encode.string val.updateRequestFileHttpBody)
   ]



type alias HttpHeader  = (String, String)

jsonDecHttpHeader : Json.Decode.Decoder ( HttpHeader )
jsonDecHttpHeader =
    Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.string))

jsonEncHttpHeader : HttpHeader -> Value
jsonEncHttpHeader  val = (\(t1,t2) -> Json.Encode.list identity [(Json.Encode.string) t1,(Json.Encode.string) t2]) val



type SignInWithGithub  = SignInWithGithub
   { signInWithGithubCode: String
   }

jsonDecSignInWithGithub : Json.Decode.Decoder ( SignInWithGithub )
jsonDecSignInWithGithub =
   Json.Decode.succeed (\psignInWithGithubCode -> (SignInWithGithub {signInWithGithubCode = psignInWithGithubCode}))
   |> required "signInWithGithubCode" (Json.Decode.string)

jsonEncSignInWithGithub : SignInWithGithub -> Value
jsonEncSignInWithGithub  (SignInWithGithub val) =
   Json.Encode.object
   [ ("signInWithGithubCode", Json.Encode.string val.signInWithGithubCode)
   ]



type alias NewScenarioFile  =
   { newScenarioFileId: UUID
   , newScenarioFileName: String
   , newScenarioFileParentNodeId: UUID
   , newScenarioFileEnvironmentId: (Maybe Int)
   }

jsonDecNewScenarioFile : Json.Decode.Decoder ( NewScenarioFile )
jsonDecNewScenarioFile =
   Json.Decode.succeed (\pnewScenarioFileId pnewScenarioFileName pnewScenarioFileParentNodeId pnewScenarioFileEnvironmentId -> {newScenarioFileId = pnewScenarioFileId, newScenarioFileName = pnewScenarioFileName, newScenarioFileParentNodeId = pnewScenarioFileParentNodeId, newScenarioFileEnvironmentId = pnewScenarioFileEnvironmentId})
   |> required "newScenarioFileId" (jsonDecUUID)
   |> required "newScenarioFileName" (Json.Decode.string)
   |> required "newScenarioFileParentNodeId" (jsonDecUUID)
   |> fnullable "newScenarioFileEnvironmentId" (Json.Decode.int)

jsonEncNewScenarioFile : NewScenarioFile -> Value
jsonEncNewScenarioFile  val =
   Json.Encode.object
   [ ("newScenarioFileId", jsonEncUUID val.newScenarioFileId)
   , ("newScenarioFileName", Json.Encode.string val.newScenarioFileName)
   , ("newScenarioFileParentNodeId", jsonEncUUID val.newScenarioFileParentNodeId)
   , ("newScenarioFileEnvironmentId", (maybeEncode (Json.Encode.int)) val.newScenarioFileEnvironmentId)
   ]



type UpdateScenarioNode  = UpdateScenarioNode
   { updateScenarioNodeName: String
   }

jsonDecUpdateScenarioNode : Json.Decode.Decoder ( UpdateScenarioNode )
jsonDecUpdateScenarioNode =
   Json.Decode.succeed (\pupdateScenarioNodeName -> (UpdateScenarioNode {updateScenarioNodeName = pupdateScenarioNodeName}))
   |> required "updateScenarioNodeName" (Json.Decode.string)

jsonEncUpdateScenarioNode : UpdateScenarioNode -> Value
jsonEncUpdateScenarioNode  (UpdateScenarioNode val) =
   Json.Encode.object
   [ ("updateScenarioNodeName", Json.Encode.string val.updateScenarioNodeName)
   ]



type alias NewScenarioFolder  =
   { newScenarioFolderId: UUID
   , newScenarioFolderParentNodeId: UUID
   , newScenarioFolderName: String
   }

jsonDecNewScenarioFolder : Json.Decode.Decoder ( NewScenarioFolder )
jsonDecNewScenarioFolder =
   Json.Decode.succeed (\pnewScenarioFolderId pnewScenarioFolderParentNodeId pnewScenarioFolderName -> {newScenarioFolderId = pnewScenarioFolderId, newScenarioFolderParentNodeId = pnewScenarioFolderParentNodeId, newScenarioFolderName = pnewScenarioFolderName})
   |> required "newScenarioFolderId" (jsonDecUUID)
   |> required "newScenarioFolderParentNodeId" (jsonDecUUID)
   |> required "newScenarioFolderName" (Json.Decode.string)

jsonEncNewScenarioFolder : NewScenarioFolder -> Value
jsonEncNewScenarioFolder  val =
   Json.Encode.object
   [ ("newScenarioFolderId", jsonEncUUID val.newScenarioFolderId)
   , ("newScenarioFolderParentNodeId", jsonEncUUID val.newScenarioFolderParentNodeId)
   , ("newScenarioFolderName", Json.Encode.string val.newScenarioFolderName)
   ]



type NewRootScenarioFolder  = NewRootScenarioFolder
   { newRootScenarioFolderId: UUID
   }

jsonDecNewRootScenarioFolder : Json.Decode.Decoder ( NewRootScenarioFolder )
jsonDecNewRootScenarioFolder =
   Json.Decode.succeed (\pnewRootScenarioFolderId -> (NewRootScenarioFolder {newRootScenarioFolderId = pnewRootScenarioFolderId}))
   |> required "newRootScenarioFolderId" (jsonDecUUID)

jsonEncNewRootScenarioFolder : NewRootScenarioFolder -> Value
jsonEncNewRootScenarioFolder  (NewRootScenarioFolder val) =
   Json.Encode.object
   [ ("newRootScenarioFolderId", jsonEncUUID val.newRootScenarioFolderId)
   ]



type alias NewRootScenarioFile  =
   { newRootScenarioFileId: UUID
   , newRootScenarioFileName: String
   , newRootScenarioFileEnvironmentId: (Maybe Int)
   }

jsonDecNewRootScenarioFile : Json.Decode.Decoder ( NewRootScenarioFile )
jsonDecNewRootScenarioFile =
   Json.Decode.succeed (\pnewRootScenarioFileId pnewRootScenarioFileName pnewRootScenarioFileEnvironmentId -> {newRootScenarioFileId = pnewRootScenarioFileId, newRootScenarioFileName = pnewRootScenarioFileName, newRootScenarioFileEnvironmentId = pnewRootScenarioFileEnvironmentId})
   |> required "newRootScenarioFileId" (jsonDecUUID)
   |> required "newRootScenarioFileName" (Json.Decode.string)
   |> fnullable "newRootScenarioFileEnvironmentId" (Json.Decode.int)

jsonEncNewRootScenarioFile : NewRootScenarioFile -> Value
jsonEncNewRootScenarioFile  val =
   Json.Encode.object
   [ ("newRootScenarioFileId", jsonEncUUID val.newRootScenarioFileId)
   , ("newRootScenarioFileName", Json.Encode.string val.newRootScenarioFileName)
   , ("newRootScenarioFileEnvironmentId", (maybeEncode (Json.Encode.int)) val.newRootScenarioFileEnvironmentId)
   ]



type ScenarioCollection  =
    ScenarioCollection UUID (List ScenarioNode)

jsonDecScenarioCollection : Json.Decode.Decoder ( ScenarioCollection )
jsonDecScenarioCollection =
    Json.Decode.lazy (\_ -> Json.Decode.map2 ScenarioCollection (Json.Decode.index 0 (jsonDecUUID)) (Json.Decode.index 1 (Json.Decode.list (jsonDecScenarioNode))))


jsonEncScenarioCollection : ScenarioCollection -> Value
jsonEncScenarioCollection (ScenarioCollection v1 v2) =
    Json.Encode.list identity [jsonEncUUID v1, (Json.Encode.list jsonEncScenarioNode) v2]



type ScenarioNode  =
    ScenarioFolder {scenarioNodeId: UUID, scenarioNodeName: String, scenarioNodeChildren: (List ScenarioNode)}
    | ScenarioFile {scenarioNodeId: UUID, scenarioNodeName: String, scenarioNodeEnvironmentId: (Maybe Int), scenarioNodeScenes: (List Scene)}

jsonDecScenarioNode : Json.Decode.Decoder ( ScenarioNode )
jsonDecScenarioNode =
    let jsonDecDictScenarioNode = Dict.fromList
            [ ("ScenarioFolder", Json.Decode.lazy (\_ -> Json.Decode.map ScenarioFolder (   Json.Decode.succeed (\pscenarioNodeId pscenarioNodeName pscenarioNodeChildren -> {scenarioNodeId = pscenarioNodeId, scenarioNodeName = pscenarioNodeName, scenarioNodeChildren = pscenarioNodeChildren})    |> required "scenarioNodeId" (jsonDecUUID)    |> required "scenarioNodeName" (Json.Decode.string)    |> required "scenarioNodeChildren" (Json.Decode.list (jsonDecScenarioNode)))))
            , ("ScenarioFile", Json.Decode.lazy (\_ -> Json.Decode.map ScenarioFile (   Json.Decode.succeed (\pscenarioNodeId pscenarioNodeName pscenarioNodeEnvironmentId pscenarioNodeScenes -> {scenarioNodeId = pscenarioNodeId, scenarioNodeName = pscenarioNodeName, scenarioNodeEnvironmentId = pscenarioNodeEnvironmentId, scenarioNodeScenes = pscenarioNodeScenes})    |> required "scenarioNodeId" (jsonDecUUID)    |> required "scenarioNodeName" (Json.Decode.string)    |> fnullable "scenarioNodeEnvironmentId" (Json.Decode.int)    |> required "scenarioNodeScenes" (Json.Decode.list (jsonDecScene)))))
            ]
        jsonDecObjectSetScenarioNode = Set.fromList ["ScenarioFolder", "ScenarioFile"]
    in  decodeSumTaggedObject "ScenarioNode" "tag" "contents" jsonDecDictScenarioNode jsonDecObjectSetScenarioNode

jsonEncScenarioNode : ScenarioNode -> Value
jsonEncScenarioNode  val =
    let keyval v = case v of
                    ScenarioFolder vs -> ("ScenarioFolder", encodeObject [("scenarioNodeId", jsonEncUUID vs.scenarioNodeId), ("scenarioNodeName", Json.Encode.string vs.scenarioNodeName), ("scenarioNodeChildren", (Json.Encode.list jsonEncScenarioNode) vs.scenarioNodeChildren)])
                    ScenarioFile vs -> ("ScenarioFile", encodeObject [("scenarioNodeId", jsonEncUUID vs.scenarioNodeId), ("scenarioNodeName", Json.Encode.string vs.scenarioNodeName), ("scenarioNodeEnvironmentId", (maybeEncode (Json.Encode.int)) vs.scenarioNodeEnvironmentId), ("scenarioNodeScenes", (Json.Encode.list jsonEncScene) vs.scenarioNodeScenes)])
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias Scene  =
   { sceneId: UUID
   , sceneRequestFileNodeId: UUID
   , scenePrescript: String
   , scenePostscript: String
   }

jsonDecScene : Json.Decode.Decoder ( Scene )
jsonDecScene =
   Json.Decode.succeed (\psceneId psceneRequestFileNodeId pscenePrescript pscenePostscript -> {sceneId = psceneId, sceneRequestFileNodeId = psceneRequestFileNodeId, scenePrescript = pscenePrescript, scenePostscript = pscenePostscript})
   |> required "sceneId" (jsonDecUUID)
   |> required "sceneRequestFileNodeId" (jsonDecUUID)
   |> required "scenePrescript" (Json.Decode.string)
   |> required "scenePostscript" (Json.Decode.string)

jsonEncScene : Scene -> Value
jsonEncScene  val =
   Json.Encode.object
   [ ("sceneId", jsonEncUUID val.sceneId)
   , ("sceneRequestFileNodeId", jsonEncUUID val.sceneRequestFileNodeId)
   , ("scenePrescript", Json.Encode.string val.scenePrescript)
   , ("scenePostscript", Json.Encode.string val.scenePostscript)
   ]



type alias NewScene  =
   { newSceneId: UUID
   , newSceneSceneNodeParentId: (Maybe UUID)
   , newSceneRequestFileNodeId: UUID
   , newScenePrescript: String
   , newScenePostscript: String
   }

jsonDecNewScene : Json.Decode.Decoder ( NewScene )
jsonDecNewScene =
   Json.Decode.succeed (\pnewSceneId pnewSceneSceneNodeParentId pnewSceneRequestFileNodeId pnewScenePrescript pnewScenePostscript -> {newSceneId = pnewSceneId, newSceneSceneNodeParentId = pnewSceneSceneNodeParentId, newSceneRequestFileNodeId = pnewSceneRequestFileNodeId, newScenePrescript = pnewScenePrescript, newScenePostscript = pnewScenePostscript})
   |> required "newSceneId" (jsonDecUUID)
   |> fnullable "newSceneSceneNodeParentId" (jsonDecUUID)
   |> required "newSceneRequestFileNodeId" (jsonDecUUID)
   |> required "newScenePrescript" (Json.Decode.string)
   |> required "newScenePostscript" (Json.Decode.string)

jsonEncNewScene : NewScene -> Value
jsonEncNewScene  val =
   Json.Encode.object
   [ ("newSceneId", jsonEncUUID val.newSceneId)
   , ("newSceneSceneNodeParentId", (maybeEncode (jsonEncUUID)) val.newSceneSceneNodeParentId)
   , ("newSceneRequestFileNodeId", jsonEncUUID val.newSceneRequestFileNodeId)
   , ("newScenePrescript", Json.Encode.string val.newScenePrescript)
   , ("newScenePostscript", Json.Encode.string val.newScenePostscript)
   ]



type alias UpdateScenarioFile  =
   { updateScenarioFileId: UUID
   , updateScenarioFileEnvironmentId: (Maybe Int)
   }

jsonDecUpdateScenarioFile : Json.Decode.Decoder ( UpdateScenarioFile )
jsonDecUpdateScenarioFile =
   Json.Decode.succeed (\pupdateScenarioFileId pupdateScenarioFileEnvironmentId -> {updateScenarioFileId = pupdateScenarioFileId, updateScenarioFileEnvironmentId = pupdateScenarioFileEnvironmentId})
   |> required "updateScenarioFileId" (jsonDecUUID)
   |> fnullable "updateScenarioFileEnvironmentId" (Json.Decode.int)

jsonEncUpdateScenarioFile : UpdateScenarioFile -> Value
jsonEncUpdateScenarioFile  val =
   Json.Encode.object
   [ ("updateScenarioFileId", jsonEncUUID val.updateScenarioFileId)
   , ("updateScenarioFileEnvironmentId", (maybeEncode (Json.Encode.int)) val.updateScenarioFileEnvironmentId)
   ]



type alias UpdateScene  =
   { updateScenePrescript: String
   , updateScenePostscript: String
   }

jsonDecUpdateScene : Json.Decode.Decoder ( UpdateScene )
jsonDecUpdateScene =
   Json.Decode.succeed (\pupdateScenePrescript pupdateScenePostscript -> {updateScenePrescript = pupdateScenePrescript, updateScenePostscript = pupdateScenePostscript})
   |> required "updateScenePrescript" (Json.Decode.string)
   |> required "updateScenePostscript" (Json.Decode.string)

jsonEncUpdateScene : UpdateScene -> Value
jsonEncUpdateScene  val =
   Json.Encode.object
   [ ("updateScenePrescript", Json.Encode.string val.updateScenePrescript)
   , ("updateScenePostscript", Json.Encode.string val.updateScenePostscript)
   ]



type RunnerConfig  = RunnerConfig
   { runnerConfigPort: Int
   }

jsonDecRunnerConfig : Json.Decode.Decoder ( RunnerConfig )
jsonDecRunnerConfig =
   Json.Decode.succeed (\prunnerConfigPort -> (RunnerConfig {runnerConfigPort = prunnerConfigPort}))
   |> required "runnerConfigPort" (Json.Decode.int)

jsonEncRunnerConfig : RunnerConfig -> Value
jsonEncRunnerConfig  (RunnerConfig val) =
   Json.Encode.object
   [ ("runnerConfigPort", Json.Encode.int val.runnerConfigPort)
   ]


getApiRequestCollection : String -> Token -> (Result Http.Error  (RequestCollection)  -> msg) -> Cmd msg
getApiRequestCollection urlBase header_X_XSRF_TOKEN toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "requestCollection"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecRequestCollection
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiScenarioCollection : String -> Token -> (Result Http.Error  (ScenarioCollection)  -> msg) -> Cmd msg
getApiScenarioCollection urlBase header_X_XSRF_TOKEN toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioCollection"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecScenarioCollection
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiEnvironment : String -> Token -> NewEnvironment -> (Result Http.Error  (Int)  -> msg) -> Cmd msg
postApiEnvironment urlBase header_X_XSRF_TOKEN body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "environment"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewEnvironment body)
            , expect =
                Http.expectJson toMsg Json.Decode.int
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiEnvironment : String -> Token -> (Result Http.Error  ((List Environment))  -> msg) -> Cmd msg
getApiEnvironment urlBase header_X_XSRF_TOKEN toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "environment"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecEnvironment))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putApiEnvironmentByEnvironmentId : String -> Token -> Int -> UpdateEnvironment -> (Result Http.Error  (())  -> msg) -> Cmd msg
putApiEnvironmentByEnvironmentId urlBase header_X_XSRF_TOKEN capture_environmentId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "environment"
                    , (capture_environmentId
                       |> String.fromInt)
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUpdateEnvironment body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteApiEnvironmentByEnvironmentId : String -> Token -> Int -> (Result Http.Error  (())  -> msg) -> Cmd msg
deleteApiEnvironmentByEnvironmentId urlBase header_X_XSRF_TOKEN capture_environmentId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "environment"
                    , (capture_environmentId
                       |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putApiEnvironmentByEnvironmentIdKeyValue : String -> Token -> Int -> (List NewKeyValue) -> (Result Http.Error  ((List KeyValue))  -> msg) -> Cmd msg
putApiEnvironmentByEnvironmentIdKeyValue urlBase header_X_XSRF_TOKEN capture_environmentId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "environment"
                    , (capture_environmentId
                       |> String.fromInt)
                    , "keyValue"
                    ]
                    params
            , body =
                Http.jsonBody ((Json.Encode.list jsonEncNewKeyValue) body)
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecKeyValue))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteApiEnvironmentByEnvironmentIdKeyValueByKeyValueId : String -> Token -> Int -> Int -> (Result Http.Error  (())  -> msg) -> Cmd msg
deleteApiEnvironmentByEnvironmentIdKeyValueByKeyValueId urlBase header_X_XSRF_TOKEN capture_environmentId capture_keyValueId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "environment"
                    , (capture_environmentId
                       |> String.fromInt)
                    , "keyValue"
                    , (capture_keyValueId |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putApiScenarioCollectionByScenarioCollectionIdScenarioNodeByScenarioNodeId : String -> Token -> UUID -> UUID -> UpdateScenarioNode -> (Result Http.Error  (())  -> msg) -> Cmd msg
putApiScenarioCollectionByScenarioCollectionIdScenarioNodeByScenarioNodeId urlBase header_X_XSRF_TOKEN capture_scenarioCollectionId capture_scenarioNodeId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioCollection"
                    , (capture_scenarioCollectionId
                       |> Uuid.toString)
                    , "scenarioNode"
                    , (capture_scenarioNodeId
                       |> Uuid.toString)
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUpdateScenarioNode body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteApiScenarioCollectionByScenarioCollectionIdScenarioNodeByScenarioNodeId : String -> Token -> UUID -> UUID -> (Result Http.Error  (())  -> msg) -> Cmd msg
deleteApiScenarioCollectionByScenarioCollectionIdScenarioNodeByScenarioNodeId urlBase header_X_XSRF_TOKEN capture_scenarioCollectionId capture_scenarioNodeId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioCollection"
                    , (capture_scenarioCollectionId
                       |> Uuid.toString)
                    , "scenarioNode"
                    , (capture_scenarioNodeId
                       |> Uuid.toString)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiScenarioCollectionByScenarioCollectionIdScenarioFile : String -> Token -> UUID -> NewScenarioFile -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiScenarioCollectionByScenarioCollectionIdScenarioFile urlBase header_X_XSRF_TOKEN capture_scenarioCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioCollection"
                    , (capture_scenarioCollectionId
                       |> Uuid.toString)
                    , "scenarioFile"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewScenarioFile body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putApiScenarioCollectionByScenarioCollectionIdScenarioFile : String -> Token -> UUID -> UpdateScenarioFile -> (Result Http.Error  (())  -> msg) -> Cmd msg
putApiScenarioCollectionByScenarioCollectionIdScenarioFile urlBase header_X_XSRF_TOKEN capture_scenarioCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioCollection"
                    , (capture_scenarioCollectionId
                       |> Uuid.toString)
                    , "scenarioFile"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUpdateScenarioFile body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiScenarioCollectionByScenarioCollectionIdRootScenarioFile : String -> Token -> UUID -> NewRootScenarioFile -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiScenarioCollectionByScenarioCollectionIdRootScenarioFile urlBase header_X_XSRF_TOKEN capture_scenarioCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioCollection"
                    , (capture_scenarioCollectionId
                       |> Uuid.toString)
                    , "rootScenarioFile"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewRootScenarioFile body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiScenarioCollectionByScenarioCollectionIdScenarioFolder : String -> Token -> UUID -> NewScenarioFolder -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiScenarioCollectionByScenarioCollectionIdScenarioFolder urlBase header_X_XSRF_TOKEN capture_scenarioCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioCollection"
                    , (capture_scenarioCollectionId
                       |> Uuid.toString)
                    , "scenarioFolder"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewScenarioFolder body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiScenarioCollectionByScenarioCollectionIdRootScenarioFolder : String -> Token -> UUID -> NewRootScenarioFolder -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiScenarioCollectionByScenarioCollectionIdRootScenarioFolder urlBase header_X_XSRF_TOKEN capture_scenarioCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioCollection"
                    , (capture_scenarioCollectionId
                       |> Uuid.toString)
                    , "rootScenarioFolder"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewRootScenarioFolder body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiScenarioNodeByScenarioNodeIdScene : String -> Token -> UUID -> NewScene -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiScenarioNodeByScenarioNodeIdScene urlBase header_X_XSRF_TOKEN capture_scenarioNodeId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioNode"
                    , (capture_scenarioNodeId
                       |> Uuid.toString)
                    , "scene"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewScene body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteApiScenarioNodeByScenarioNodeIdSceneBySceneId : String -> Token -> UUID -> UUID -> (Result Http.Error  (())  -> msg) -> Cmd msg
deleteApiScenarioNodeByScenarioNodeIdSceneBySceneId urlBase header_X_XSRF_TOKEN capture_scenarioNodeId capture_sceneId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioNode"
                    , (capture_scenarioNodeId
                       |> Uuid.toString)
                    , "scene"
                    , (capture_sceneId |> Uuid.toString)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putApiScenarioNodeByScenarioNodeIdSceneBySceneId : String -> Token -> UUID -> UUID -> UpdateScene -> (Result Http.Error  (())  -> msg) -> Cmd msg
putApiScenarioNodeByScenarioNodeIdSceneBySceneId urlBase header_X_XSRF_TOKEN capture_scenarioNodeId capture_sceneId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "scenarioNode"
                    , (capture_scenarioNodeId
                       |> Uuid.toString)
                    , "scene"
                    , (capture_sceneId |> Uuid.toString)
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUpdateScene body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId : String -> Token -> Int -> UUID -> UpdateRequestNode -> (Result Http.Error  (())  -> msg) -> Cmd msg
putApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId urlBase header_X_XSRF_TOKEN capture_requestCollectionId capture_requestNodeId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "requestCollection"
                    , (capture_requestCollectionId
                       |> String.fromInt)
                    , "requestNode"
                    , (capture_requestNodeId
                       |> Uuid.toString)
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUpdateRequestNode body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId : String -> Token -> Int -> UUID -> (Result Http.Error  (())  -> msg) -> Cmd msg
deleteApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId urlBase header_X_XSRF_TOKEN capture_requestCollectionId capture_requestNodeId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "requestCollection"
                    , (capture_requestCollectionId
                       |> String.fromInt)
                    , "requestNode"
                    , (capture_requestNodeId
                       |> Uuid.toString)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiRequestCollectionByRequestCollectionIdRequestFile : String -> Token -> Int -> NewRequestFile -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiRequestCollectionByRequestCollectionIdRequestFile urlBase header_X_XSRF_TOKEN capture_requestCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "requestCollection"
                    , (capture_requestCollectionId
                       |> String.fromInt)
                    , "requestFile"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewRequestFile body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiRequestCollectionByRequestCollectionIdRootRequestFile : String -> Token -> Int -> NewRootRequestFile -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiRequestCollectionByRequestCollectionIdRootRequestFile urlBase header_X_XSRF_TOKEN capture_requestCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "requestCollection"
                    , (capture_requestCollectionId
                       |> String.fromInt)
                    , "rootRequestFile"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewRootRequestFile body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putApiRequestCollectionByRequestCollectionIdByRequestNodeId : String -> Token -> Int -> UUID -> UpdateRequestFile -> (Result Http.Error  (())  -> msg) -> Cmd msg
putApiRequestCollectionByRequestCollectionIdByRequestNodeId urlBase header_X_XSRF_TOKEN capture_requestCollectionId capture_requestNodeId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "requestCollection"
                    , (capture_requestCollectionId
                       |> String.fromInt)
                    , (capture_requestNodeId
                       |> Uuid.toString)
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUpdateRequestFile body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiRequestCollectionByRequestCollectionIdRequestFolder : String -> Token -> Int -> NewRequestFolder -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiRequestCollectionByRequestCollectionIdRequestFolder urlBase header_X_XSRF_TOKEN capture_requestCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "requestCollection"
                    , (capture_requestCollectionId
                       |> String.fromInt)
                    , "requestFolder"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewRequestFolder body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiRequestCollectionByRequestCollectionIdRootRequestFolder : String -> Token -> Int -> NewRootRequestFolder -> (Result Http.Error  (())  -> msg) -> Cmd msg
postApiRequestCollectionByRequestCollectionIdRootRequestFolder urlBase header_X_XSRF_TOKEN capture_requestCollectionId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "requestCollection"
                    , (capture_requestCollectionId
                       |> String.fromInt)
                    , "rootRequestFolder"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewRootRequestFolder body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiSessionSignInWithGithub : String -> SignInWithGithub -> (Result Http.Error  (Session)  -> msg) -> Cmd msg
postApiSessionSignInWithGithub urlBase body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "session"
                    , "signInWithGithub"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncSignInWithGithub body)
            , expect =
                Http.expectJson toMsg jsonDecSession
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteApiSessionSignout : String -> (Result Http.Error  (Session)  -> msg) -> Cmd msg
deleteApiSessionSignout urlBase toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "session"
                    , "signout"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecSession
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiSessionWhoami : String -> Token -> (Result Http.Error  (Session)  -> msg) -> Cmd msg
getApiSessionWhoami urlBase header_X_XSRF_TOKEN toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "X-XSRF-TOKEN") (Just header_X_XSRF_TOKEN)
                    ]
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "session"
                    , "whoami"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecSession
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiAccountResetVisitorAccount : String -> (Result Http.Error  (())  -> msg) -> Cmd msg
getApiAccountResetVisitorAccount urlBase toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "account"
                    , "resetVisitorAccount"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiHealth : String -> (Result Http.Error  (AppHealth)  -> msg) -> Cmd msg
getApiHealth urlBase toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "health"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecAppHealth
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getApiConfig : String -> (Result Http.Error  (RunnerConfig)  -> msg) -> Cmd msg
getApiConfig urlBase toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "config"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecRunnerConfig
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
