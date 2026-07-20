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
import Env
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
import Proquint
import Quantity exposing (Quantity)
import Random
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material
import Scene3d.Mesh
import SeqDict
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
      , seed = Random.initialSeed 0
      , waitingForFriend = SeqDict.empty
      , adminClient = Nothing
      }
    , Random.independentSeed
        |> Random.generate SeedInitialized
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
        SeedInitialized seed ->
            ( { model | seed = seed }, Cmd.none )

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
                                    , redTowersRemaining = game.redTowersRemaining
                                    , blueTowersRemaining = game.blueTowersRemaining
                                    }
                                )
                            , Lamdera.sendToFrontend left OpponentConnected
                            ]
                        )

            else
                ( model, Cmd.none )

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
    let
        ( blueTowers, redTowers ) =
            countAllTowers initBodies
    in
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
              , redTowersRemaining = redTowers
              , blueTowersRemaining = blueTowers
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
                                    Just
                                        ( left
                                        , right
                                        , runTurn
                                            { game
                                                | stage = Simulating
                                                , bodies = applyImpulseToBodies game.turn elevationF rotationF forceF game.bodies
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
                            , redTowersRemaining = game.redTowersRemaining
                            , blueTowersRemaining = game.blueTowersRemaining
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

        PlayWithStranger ->
            case model.waiting of
                Nothing ->
                    ( { model | waiting = Just sessionId }
                    , Lamdera.sendToFrontend sessionId BeginWaitingForStranger
                    )

                Just waitingId ->
                    startMatch sessionId waitingId model

        HostFriend ->
            let
                ( gameId, seed ) =
                    Random.step
                        (Random.map Proquint.toString
                            Proquint.randomGenerator
                        )
                        model.seed
            in
            ( { model
                | seed = seed
                , waitingForFriend = SeqDict.insert gameId sessionId model.waitingForFriend
              }
            , Lamdera.sendToFrontend sessionId (BeginWaitingForFriend gameId)
            )

        JoinFriend joinCode ->
            if joinCode == Env.adminPassword then
                ( { model | adminClient = Just clientId }
                , Lamdera.sendToFrontend clientId Admin_LoggedIn
                )

            else
                case SeqDict.get joinCode model.waitingForFriend of
                    Nothing ->
                        ( model
                        , Lamdera.sendToFrontend sessionId UnknownJoinCode
                        )

                    Just friendSessionId ->
                        startMatch sessionId friendSessionId model

        AbandonWaiting ->
            case model.waiting of
                Just waitingId ->
                    if waitingId == sessionId then
                        ( { model | waiting = Nothing }, Cmd.none )

                    else if SeqDict.member sessionId model.waitingForFriend then
                        ( { model | waitingForFriend = SeqDict.remove sessionId model.waitingForFriend }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    if SeqDict.member sessionId model.waitingForFriend then
                        ( { model | waitingForFriend = SeqDict.remove sessionId model.waitingForFriend }, Cmd.none )

                    else
                        ( model, Cmd.none )

        LeaveMatchRequested ->
            let
                ( filteredRooms, maybeGame ) =
                    listFilterFind
                        (\( left, right, game ) ->
                            if sessionId == left then
                                Just right

                            else if sessionId == right then
                                Just left

                            else
                                Nothing
                        )
                        model.rooms
            in
            ( { model | rooms = filteredRooms }
            , case maybeGame of
                Nothing ->
                    Cmd.none

                Just otherPlayer ->
                    Lamdera.sendToFrontend otherPlayer OpponentLeft
            )

        --
        Admin_ClearAllMatches ->
            case model.adminClient of
                Nothing ->
                    ( model, Cmd.none )

                Just adminId ->
                    if adminId == clientId then
                        ( { model
                            | waiting = Nothing
                            , rooms = []
                            , hasLeft = SeqSet.empty
                            , waitingForFriend = SeqDict.empty
                          }
                        , Cmd.batch
                            ((case model.waiting of
                                Nothing ->
                                    Cmd.none

                                Just waitingId ->
                                    Lamdera.sendToFrontend waitingId Admin_ForcedReset
                             )
                                :: List.concatMap
                                    (\( left, right, _ ) ->
                                        [ Lamdera.sendToFrontend left Admin_ForcedReset
                                        , Lamdera.sendToFrontend right Admin_ForcedReset
                                        ]
                                    )
                                    model.rooms
                                ++ SeqDict.foldl
                                    (\_ friendId -> (::) (Lamdera.sendToFrontend friendId Admin_ForcedReset))
                                    []
                                    model.waitingForFriend
                            )
                        )

                    else
                        ( model, Cmd.none )


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
    if steppedGame.stage /= Simulating then
        steppedGame

    else
        runTurnHelper initialTurn steppedGame



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


listFilterFind : (a -> Maybe b) -> List a -> ( List a, Maybe b )
listFilterFind pred list =
    listFilterFindHelper pred list []


listFilterFindHelper : (a -> Maybe b) -> List a -> List a -> ( List a, Maybe b )
listFilterFindHelper pred toCheck checked =
    case toCheck of
        [] ->
            ( checked, Nothing )

        next :: rest ->
            case pred next of
                Just b ->
                    ( checked ++ toCheck, Just b )

                Nothing ->
                    listFilterFindHelper pred rest (next :: checked)
