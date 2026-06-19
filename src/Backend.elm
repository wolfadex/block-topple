module Backend exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cone3d exposing (Cone3d)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode exposing (Decoder)
import Lamdera exposing (ClientId, SessionId, onConnect)
import Length exposing (Length, Meters)
import Obj.Decode
import Parameter1d
import Physics exposing (Body, BodyCoordinates, WorldCoordinates, onEarth)
import Physics.Constraint exposing (Constraint)
import Physics.Material
import Physics.Shape
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Process
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material
import Scene3d.Mesh
import SeqSet
import Sphere3d exposing (Sphere3d)
import String exposing (left)
import Task
import Timestep exposing (Timestep)
import TriangularMesh exposing (TriangularMesh)
import Types exposing (..)
import Url exposing (Url)
import Vector3d exposing (Vector3d)
import WebGL.Texture


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { waiting = Nothing
      , rooms = []
      , hasLeft = SeqSet.empty
      }
    , Cmd.none
    )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect OnConnect
        , Lamdera.onDisconnect OnDisconnect
        ]


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        OnConnect sessionId clientId ->
            if SeqSet.member sessionId model.hasLeft then
                case
                    listFindMap
                        (\( left, right, game ) ->
                            if sessionId == left || sessionId == right then
                                Just game

                            else
                                Nothing
                        )
                        model.rooms
                of
                    Nothing ->
                        ( { model
                            | hasLeft = SeqSet.remove sessionId model.hasLeft
                            , waiting = Just sessionId
                          }
                        , Cmd.none
                        )

                    Just game ->
                        ( { model
                            | hasLeft = SeqSet.remove sessionId model.hasLeft
                          }
                        , let
                            ( ( left, leftTurn ), ( _, rightTurn ) ) =
                                game.players
                          in
                          Cmd.batch
                            [ Lamdera.sendToFrontend sessionId
                                (GameRejoined
                                    { yourColor =
                                        if sessionId == left then
                                            leftTurn

                                        else
                                            rightTurn
                                    , bodies = game.bodies
                                    , contacts = game.contacts
                                    , turn = game.turn
                                    , stage = game.stage
                                    , elapsed = game.elapsed
                                    , timestep = game.timestep
                                    }
                                )
                            , Lamdera.sendToFrontend left OpponentConnected
                            ]
                        )

            else
                case model.waiting of
                    Nothing ->
                        ( { model | waiting = Just sessionId }
                        , Cmd.none
                        )

                    Just waitingId ->
                        startMatch sessionId waitingId model

        OnDisconnect sessionId clientId ->
            case model.waiting of
                Just waitingId ->
                    if waitingId == sessionId then
                        ( { model | waiting = Nothing }
                        , Cmd.none
                        )

                    else
                        notifyUserHasDisconnected sessionId model

                Nothing ->
                    notifyUserHasDisconnected sessionId model

        UserHasLeft sessionId ->
            let
                ( rooms, otherSession ) =
                    removeFromRoom sessionId model.rooms
            in
            case model.waiting of
                Nothing ->
                    ( { model
                        | hasLeft = SeqSet.remove sessionId model.hasLeft
                        , rooms = rooms
                        , waiting = otherSession
                      }
                    , case otherSession of
                        Nothing ->
                            Cmd.none

                        Just sesId ->
                            Lamdera.sendToFrontend sesId OtherPlayerLeft
                    )

                Just waitingId ->
                    startMatch sessionId waitingId model

        GameUpdateElapsed left right gameDetails ->
            ( model
            , Cmd.batch
                [ Lamdera.sendToFrontend left (TurnChange gameDetails)
                , Lamdera.sendToFrontend right (TurnChange gameDetails)
                ]
            )


notifyUserHasDisconnected : SessionId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
notifyUserHasDisconnected sessionId model =
    let
        otherPlayer =
            listFindMap
                (\( left, right, _ ) ->
                    if sessionId == left then
                        Just right

                    else if sessionId == right then
                        Just left

                    else
                        Nothing
                )
                model.rooms
    in
    case otherPlayer of
        Nothing ->
            ( { model | hasLeft = SeqSet.remove sessionId model.hasLeft }
            , Cmd.none
            )

        Just otherId ->
            ( { model | hasLeft = SeqSet.insert sessionId model.hasLeft }
            , Cmd.batch
                [ Process.sleep (30 * 1000)
                    |> Task.perform (\() -> UserHasLeft sessionId)
                , Lamdera.sendToFrontend otherId OpponentDisconnected
                ]
            )


startMatch : SessionId -> SessionId -> BackendModel -> ( BackendModel, Cmd BackendMsg )
startMatch sessionId waitingId model =
    ( { model
        | waiting = Nothing
        , rooms =
            ( waitingId
            , sessionId
            , { players = ( ( waitingId, Red ), ( sessionId, Blue ) )
              , bodies = initBodies
              , contacts = Physics.emptyContacts
              , turn = Red
              , stage = Aiming
              , elapsed = Duration.seconds 0
              , timestep =
                    Timestep.init
                        { duration = Duration.seconds (1 / 120)
                        , maxSteps = 2
                        }
              }
            )
                :: model.rooms
      }
    , Cmd.batch
        [ Lamdera.sendToFrontend waitingId (GameStarted Red)
        , Lamdera.sendToFrontend sessionId (GameStarted Blue)
        ]
    )


removeFromRoom : SessionId -> List ( SessionId, SessionId, Game ) -> ( List ( SessionId, SessionId, Game ), Maybe SessionId )
removeFromRoom toRemoveId rooms =
    removeFromRoomHelper toRemoveId [] rooms


removeFromRoomHelper : SessionId -> List ( SessionId, SessionId, Game ) -> List ( SessionId, SessionId, Game ) -> ( List ( SessionId, SessionId, Game ), Maybe SessionId )
removeFromRoomHelper toRemoveId checkedRooms rooms =
    case rooms of
        [] ->
            ( checkedRooms, Nothing )

        (( left, right, _ ) as room) :: restRooms ->
            if toRemoveId == left then
                ( checkedRooms ++ restRooms, Just right )

            else if toRemoveId == right then
                ( checkedRooms ++ restRooms, Just left )

            else
                removeFromRoomHelper toRemoveId (room :: checkedRooms) restRooms


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        Fire elevationF rotationF forceF ->
            let
                ( rooms, updatedGame ) =
                    listUpdateWhen
                        (\(( left, right, game ) as room) ->
                            if sessionId == left || sessionId == right then
                                let
                                    ( ( _, leftTurn ), ( _, rightTurn ) ) =
                                        game.players
                                in
                                if sessionId == left && game.turn == leftTurn || sessionId == right && game.turn == rightTurn then
                                    let
                                        impulse =
                                            Vector3d.withLength
                                                (Quantity.times (Duration.seconds 0.005)
                                                    (Force.meganewtons forceF)
                                                )
                                                ((case game.turn of
                                                    Red ->
                                                        Direction3d.negativeX

                                                    Blue ->
                                                        Direction3d.positiveX
                                                 )
                                                    |> Direction3d.rotateAround
                                                        (case game.turn of
                                                            Red ->
                                                                Direction3d.positiveY

                                                            Blue ->
                                                                Direction3d.negativeY
                                                        )
                                                        (Angle.degrees elevationF)
                                                    |> Direction3d.rotateAround
                                                        Direction3d.negativeZ
                                                        (Angle.degrees rotationF)
                                                )
                                    in
                                    Just
                                        ( left
                                        , right
                                        , runTurn
                                            { game
                                                | stage = Simulating
                                                , bodies =
                                                    List.map
                                                        (\( id, body ) ->
                                                            ( id
                                                            , case id of
                                                                RedBall _ _ ->
                                                                    case game.turn of
                                                                        Red ->
                                                                            Physics.applyImpulse
                                                                                impulse
                                                                                (Physics.centerOfMass body
                                                                                    |> Maybe.withDefault Point3d.origin
                                                                                    |> Point3d.translateBy
                                                                                        (Vector3d.scaleTo ballRadius impulse)
                                                                                )
                                                                                body

                                                                        Blue ->
                                                                            body

                                                                BlueBall _ _ ->
                                                                    case game.turn of
                                                                        Blue ->
                                                                            Physics.applyImpulse
                                                                                impulse
                                                                                (Physics.centerOfMass body
                                                                                    |> Maybe.withDefault Point3d.origin
                                                                                    |> Point3d.translateBy
                                                                                        (Vector3d.scaleTo ballRadius impulse)
                                                                                )
                                                                                body

                                                                        Red ->
                                                                            body

                                                                _ ->
                                                                    body
                                                            )
                                                        )
                                                        game.bodies
                                            }
                                        )

                                else
                                    Nothing

                            else
                                Nothing
                        )
                        model.rooms
            in
            ( { model
                | rooms = rooms
              }
            , case updatedGame of
                Just ( left, right, game ) ->
                    let
                        gameDetails =
                            { bodies = game.bodies
                            , contacts = game.contacts
                            , turn = game.turn
                            , stage = game.stage
                            }
                    in
                    Cmd.batch
                        [ Process.sleep (5 * 1000)
                            |> Task.perform (\() -> GameUpdateElapsed left right gameDetails)
                        , Lamdera.sendToFrontend
                            (if sessionId == left then
                                right

                             else
                                left
                            )
                            (OtherPlayerFired elevationF rotationF forceF)
                        ]

                Nothing ->
                    Cmd.none
            )


runTurn : Game -> Game
runTurn game =
    runTurnHelper game.turn game


runTurnHelper : Turn -> Game -> Game
runTurnHelper initialTurn game =
    let
        steppedGame =
            Timestep.advance simulateStep
                (Duration.seconds (1 / 120))
                game
    in
    if steppedGame.turn /= initialTurn then
        steppedGame

    else
        runTurnHelper initialTurn steppedGame


simulateStep : Game -> Game
simulateStep model =
    let
        ( simulated, newContacts ) =
            Physics.simulate
                { onEarth
                    | contacts = model.contacts
                    , duration = Timestep.duration model.timestep
                }
                model.bodies

        newElapsed =
            Debug.log "newElapsed" <|
                case model.stage of
                    Aiming ->
                        model.elapsed

                    Simulating ->
                        Quantity.plus model.elapsed (Timestep.duration model.timestep)

        newModel =
            { model
                | bodies = simulated
                , contacts = newContacts
                , elapsed = newElapsed
            }
    in
    if newElapsed |> Quantity.greaterThanOrEqualTo (Duration.seconds 5) then
        { newModel
            | stage = Aiming
            , turn =
                case newModel.turn of
                    Red ->
                        Blue

                    Blue ->
                        Red
            , elapsed = Duration.seconds 0
            , bodies =
                (case newModel.turn of
                    Red ->
                        initBlueBall

                    Blue ->
                        initRedBall
                )
                    :: List.filterMap
                        (\( id, body ) ->
                            case id of
                                RedBall _ _ ->
                                    Nothing

                                BlueBall _ _ ->
                                    Nothing

                                _ ->
                                    Just
                                        ( id
                                        , body
                                        )
                        )
                        newModel.bodies
        }

    else
        newModel



--


listUpdateWhen : (a -> Maybe a) -> List a -> ( List a, Maybe a )
listUpdateWhen fn list =
    listUpdateWhenHelp fn [] list


listUpdateWhenHelp : (a -> Maybe a) -> List a -> List a -> ( List a, Maybe a )
listUpdateWhenHelp fn done list =
    case list of
        [] ->
            ( done, Nothing )

        next :: rest ->
            case fn next of
                Just a ->
                    ( a :: done ++ rest, Just a )

                Nothing ->
                    listUpdateWhenHelp fn (next :: done) rest


listFindMap : (a -> Maybe b) -> List a -> Maybe b
listFindMap pred list =
    case list of
        [] ->
            Nothing

        next :: rest ->
            case pred next of
                Just b ->
                    Just b

                Nothing ->
                    listFindMap pred rest
