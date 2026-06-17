module Server exposing (main)

import Worker
import Worker.Capabilities as Caps
import Worker.HttpServer as Http


type alias Model =
    { servingAllowed : Maybe Caps.HttpServer
    }


type Msg
    = GotRequest Http.Request


init : Worker.Env -> ( Model, Cmd Msg )
init flags =
    ( { servingAllowed = flags.capabilities.httpServer }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRequest req ->
            ( model
            , Http.respond
                { connectionId = req.connectionId
                , streamId = req.streamId
                , status = 200
                , contentType = "text/plain; charset=utf-8"
                , body = "👋"
                }
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.servingAllowed of
        Just cap ->
            Http.onRequest cap GotRequest

        Nothing ->
            Sub.none


main : Worker.Program Model Msg
main =
    Worker.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
