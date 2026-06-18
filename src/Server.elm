module Server exposing (main)

import Dict exposing (Dict)
import Fs
import Fs.Location
import Random
import Resources
import Task
import UUID exposing (UUID)
import Worker
import Worker.Capabilities
import Worker.HttpServer


type alias Model =
    { servingAllowed : Maybe Worker.Capabilities.HttpServer
    , fsAllowed : Maybe Fs.FileSystem
    , requests : Dict String String
    , monitor : Maybe SseSub
    , uuidSeeds : UUID.Seeds
    }



-- Streams and connections are properties of HTTP/2 and HTTP/3
-- but in HTTP/1.1 stream is just ignored.


type alias SseSub =
    { connectionId : Int
    , streamId : Int
    }


init : Worker.Env -> ( Model, Cmd Msg )
init flags =
    ( { servingAllowed = flags.capabilities.httpServer
      , fsAllowed =
            case Fs.fromFlags flags of
                Fs.Sandboxed fs ->
                    Just fs

                Fs.NoAccess ->
                    Nothing
      , requests = Dict.empty
      , monitor = Nothing
      , uuidSeeds =
            { seed1 = Random.initialSeed 0
            , seed2 = Random.initialSeed 0
            , seed3 = Random.initialSeed 0
            , seed4 = Random.initialSeed 0
            }
      }
    , Random.map4
        (\seed1 seed2 seed3 seed4 ->
            { seed1 = seed1
            , seed2 = seed2
            , seed3 = seed3
            , seed4 = seed4
            }
        )
        Random.independentSeed
        Random.independentSeed
        Random.independentSeed
        Random.independentSeed
        |> Random.generate GotUuidInitNumbers
    )


type Msg
    = GotRequest Worker.HttpServer.Request
    | GotStaticFile Worker.HttpServer.Request (Result Fs.FsError String)
    | GotUuidInitNumbers UUID.Seeds


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUuidInitNumbers uuidSeeds ->
            ( { model
                | uuidSeeds = uuidSeeds
              }
            , Cmd.none
            )

        GotRequest req ->
            requestHandler model req

        GotStaticFile req (Ok file) ->
            ( model
            , sendTextFile req file (mediaTypeForRoute req.path)
            )

        GotStaticFile req (Err _) ->
            ( model
            , send404 req
            )


requestHandler : Model -> Worker.HttpServer.Request -> ( Model, Cmd Msg )
requestHandler model req =
    if req.path == "/_follow" then
        ( { model | monitor = Just { connectionId = req.connectionId, streamId = req.streamId } }
        , Worker.HttpServer.startSse
            { connectionId = req.connectionId
            , streamId = req.streamId
            }
        )

    else if req.path == "/_routes" then
        ( model, allRoutes req model )

    else if req.path == "/_settings" then
        ( model, serveFile model req ui )

    else if req.path == "/_settings/requestbin.js" then
        ( model, serveFile model req uijs )
        -- else if req.path == settingsPath then
        --     ( model, serveResource model req (Location.display (Location.fromFile ui)) )

    else if req.path == settingsPath then
        ( model, serveResource model req "requestbin.html" )

    else if String.startsWith settingsPath req.path then
        ( model, serveResource model req (String.dropLeft (String.length settingsPath + 1) req.path) )

    else if req.path == "/_resources" then
        ( model, sendTextFile req (String.join "\n" Resources.paths) "text/plain; charset=utf-8" )

    else if req.path == "/" then
        ( model, serveFile model req index )

    else if pathHasExtension req then
        ( model, serveFile model req (Fs.Location.file (String.dropLeft 1 req.path)) )

    else
        echo req model


pathHasExtension : Worker.HttpServer.Request -> Bool
pathHasExtension req =
    case String.split "." req.path |> List.reverse of
        _ :: _ :: _ ->
            True

        _ ->
            False


settingsPath : String
settingsPath =
    "/_console"


index : Fs.Location.File
index =
    Fs.Location.file "index.html"


ui : Fs.Location.File
ui =
    Fs.Location.file "requestbin.html"


uijs : Fs.Location.File
uijs =
    Fs.Location.file "requestbin.js"


serveResource : Model -> Worker.HttpServer.Request -> String -> Cmd Msg
serveResource model req resourceName =
    case Resources.readText resourceName of
        Just body ->
            sendTextFile req body (mediaType resourceName)

        _ ->
            send404 req


serveFile : Model -> Worker.HttpServer.Request -> Fs.Location.File -> Cmd Msg
serveFile model req file =
    case model.fsAllowed of
        Just fs ->
            Fs.readTextFile fs file
                |> Task.attempt (GotStaticFile req)

        Nothing ->
            send404 req


send404 : Worker.HttpServer.Request -> Cmd Msg
send404 req =
    Worker.HttpServer.respond
        { connectionId = req.connectionId
        , streamId = req.streamId
        , status = 404
        , contentType = "text/plain; charset=utf-8"
        , body = "Not found"
        }


allRoutes : Worker.HttpServer.Request -> Model -> Cmd Msg
allRoutes req model =
    Worker.HttpServer.respond
        { connectionId = req.connectionId
        , streamId = req.streamId
        , status = 200
        , contentType = "text/plain; charset=utf-8"
        , body =
            String.join "\n\n" (List.map (\( k, v ) -> k ++ "|" ++ v) (Dict.toList model.requests))
        }


ssePing : Model -> String -> String -> Cmd Msg
ssePing model message payload =
    case model.monitor of
        Just sub ->
            Worker.HttpServer.sendSse
                { connectionId = sub.connectionId
                , streamId = sub.streamId
                , event = message
                , data = payload
                }

        Nothing ->
            Cmd.none


echo : Worker.HttpServer.Request -> Model -> ( Model, Cmd Msg )
echo req model =
    let
        headers =
            String.join "\n" (List.map (\( k, v ) -> k ++ ": " ++ v) req.headers)

        -- GET requests have no body
        store body =
            let
                bodyString =
                    if body == "" then
                        "👋"

                    else
                        body
            in
            ( { model | requests = Dict.insert req.path bodyString model.requests }
            , bodyString
            , ssePing model "new-route" (req.method ++ ", " ++ req.path ++ "\n" ++ bodyString)
            )

        ( model_, responseBody, newRoute ) =
            -- PUT always (over)writes the stored payload; everything else
            -- echoes a known route or stores a newly discovered one.
            if req.method == "PUT" then
                store req.body

            else
                case Dict.get req.path model.requests of
                    Just r ->
                        ( model, r, Cmd.none )

                    Nothing ->
                        store req.body
    in
    ( model_
    , Cmd.batch
        [ Worker.HttpServer.respond
            { connectionId = req.connectionId
            , streamId = req.streamId
            , status = 200
            , contentType = "text/plain; charset=utf-8"
            , body = responseBody
            }
        , ssePing model_ "request" (req.method ++ " " ++ req.path ++ "\n" ++ headers ++ "\n" ++ responseBody)
        , newRoute
        ]
    )


{-| Extensionless UI routes serve html files; without this the browser is
told `application/octet-stream` and downloads instead of rendering.
Static file serving is boring, told you!
-}
mediaTypeForRoute : String -> String
mediaTypeForRoute path =
    if path == "/" then
        mediaType "index.html"

    else if path == settingsPath || path == "/_settings" then
        mediaType "requestbin.html"

    else
        mediaType path


mediaType : String -> String
mediaType path =
    case String.split "." path |> List.reverse of
        "html" :: _ ->
            "text/html; charset=utf-8"

        "txt" :: _ ->
            "text/plain; charset=utf-8"

        "json" :: _ ->
            "application/json; charset=utf-8"

        "js" :: _ ->
            "application/javascript; charset=utf-8"

        _ ->
            "application/octet-stream"


sendTextFile : Worker.HttpServer.Request -> String -> String -> Cmd Msg
sendTextFile req fileBody contentType =
    Worker.HttpServer.respond
        { connectionId = req.connectionId
        , streamId = req.streamId
        , status = 200
        , contentType = contentType
        , body = fileBody
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.servingAllowed of
        Just cap ->
            Worker.HttpServer.onRequest cap GotRequest

        Nothing ->
            Sub.none


main : Worker.Program Model Msg
main =
    Worker.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
