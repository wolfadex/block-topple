module Frontend exposing (..)

import Angle exposing (Angle)
import AppUrl exposing (AppUrl)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cone3d exposing (Cone3d)
import Css
import Cylinder3d exposing (Cylinder3d)
import Dict
import Direction3d
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode exposing (Decoder)
import Lamdera
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
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material
import Scene3d.Mesh
import Sphere3d exposing (Sphere3d)
import Task
import Timestep exposing (Timestep)
import TriangularMesh exposing (TriangularMesh)
import Types exposing (..)
import Url exposing (Url)
import Vector3d exposing (Vector3d)
import WebGL.Texture


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


urlToPage : Url -> Page
urlToPage url =
    let
        appUrl =
            AppUrl.fromUrl url
    in
    case Dict.get "g" appUrl.queryParameters of
        Just (gameToJoin :: _) ->
            Home gameToJoin False

        _ ->
            Home "" False


init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , page = urlToPage url
      , dimensions = ( Pixels.int 0, Pixels.int 0 )

      --
      , boxMesh = Nothing
      , boxMaterialRed = Nothing
      , boxMaterialBlue = Nothing
      , cylinderMesh = Nothing

      --
      , letterBlocks = Dict.empty
      }
    , Cmd.batch
        [ Task.perform
            (\{ viewport } -> Resize (round viewport.width) (round viewport.height))
            Browser.Dom.getViewport
        , loadBox
        , loadCylinder
        ]
    )


loadBox : Cmd FrontendMsg
loadBox =
    Cmd.batch
        [ Http.get
            { url = "/assets/box.obj"
            , expect =
                Obj.Decode.expectObj BoxMeshLoaded
                    Length.meters
                    (Obj.Decode.map Scene3d.Mesh.texturedFaces
                        (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
                    )
            }
        , Scene3d.Material.load "/assets/box_red.png"
            |> Task.attempt BoxRedTextureLoaded
        , Scene3d.Material.load "/assets/box_blue.png"
            |> Task.attempt BoxBlueTextureLoaded

        --
        , letters
            |> List.map
                (\letter ->
                    Task.map2 Tuple.pair
                        (Http.task
                            { method = "GET"
                            , headers = []
                            , url = "/assets/box_letter_" ++ String.fromChar letter ++ ".obj"
                            , body = Http.emptyBody
                            , resolver =
                                Http.stringResolver
                                    (\response ->
                                        case response of
                                            Http.BadUrl_ url ->
                                                Err (Http.BadUrl url)

                                            Http.Timeout_ ->
                                                Err Http.Timeout

                                            Http.NetworkError_ ->
                                                Err Http.NetworkError

                                            Http.BadStatus_ metadata _ ->
                                                Err (Http.BadStatus metadata.statusCode)

                                            Http.GoodStatus_ _ body ->
                                                let
                                                    units =
                                                        Length.meters

                                                    decoder =
                                                        Obj.Decode.map Scene3d.Mesh.texturedFaces
                                                            (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
                                                in
                                                case Obj.Decode.decodeString units decoder body of
                                                    Ok value ->
                                                        Ok value

                                                    Err string ->
                                                        Err (Http.BadBody string)
                                    )
                            , timeout = Nothing
                            }
                            |> Task.mapError Debug.toString
                        )
                        (Scene3d.Material.load ("/assets/box_red_letter_" ++ String.fromChar letter ++ ".png")
                            |> Task.mapError Debug.toString
                        )
                        |> Task.attempt (LetterLoaded letter)
                )
            |> Cmd.batch
        ]


loadCylinder : Cmd FrontendMsg
loadCylinder =
    Http.get
        { url = "/assets/cylinder.obj"
        , expect =
            Obj.Decode.expectObj CylinderMeshLoaded
                Length.meters
                (Obj.Decode.map Scene3d.Mesh.texturedFaces
                    (Obj.Decode.texturedFacesIn Frame3d.atOrigin)
                )
        }


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resize
        , let
            doTick =
                case model.page of
                    AdminView ->
                        False

                    Home _ _ ->
                        False

                    Waiting _ ->
                        False

                    InGame gameModel ->
                        (gameModel.stage == Simulating)
                            || (gameModel.opponentDisconnected /= Nothing)
          in
          if doTick then
            Browser.Events.onAnimationFrameDelta (\d -> GameMessage (Tick (Duration.milliseconds d)))

          else
            Sub.none
        ]


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        Resize width height ->
            ( { model | dimensions = ( Pixels.int width, Pixels.int height ) }
            , Cmd.none
            )

        --
        BoxMeshLoaded (Err err) ->
            -- Debug.todo (Debug.toString err)
            ( model, Cmd.none )

        BoxMeshLoaded (Ok boxMesh) ->
            ( { model | boxMesh = Just ( boxMesh, Scene3d.Mesh.shadow boxMesh ) }
            , Cmd.none
            )

        BoxRedTextureLoaded (Err err) ->
            -- Debug.todo (Debug.toString err)
            ( model, Cmd.none )

        BoxRedTextureLoaded (Ok texture) ->
            ( { model | boxMaterialRed = Just (Scene3d.Material.texturedMatte texture) }
            , Cmd.none
            )

        BoxBlueTextureLoaded (Err err) ->
            -- Debug.todo (Debug.toString err)
            ( model, Cmd.none )

        BoxBlueTextureLoaded (Ok texture) ->
            ( { model | boxMaterialBlue = Just (Scene3d.Material.texturedMatte texture) }
            , Cmd.none
            )

        CylinderMeshLoaded (Err err) ->
            -- Debug.todo (Debug.toString err)
            ( model, Cmd.none )

        CylinderMeshLoaded (Ok cylinderMesh) ->
            ( { model | cylinderMesh = Just ( cylinderMesh, Scene3d.Mesh.shadow cylinderMesh ) }
            , Cmd.none
            )

        LetterLoaded letter (Err err) ->
            -- Debug.todo (letter ++ ": " ++ Debug.toString err)
            ( model, Cmd.none )

        LetterLoaded letter (Ok ( mesh, texture )) ->
            ( { model
                | letterBlocks =
                    Dict.insert letter
                        ( ( mesh, Scene3d.Mesh.shadow mesh )
                        , Scene3d.Material.texturedMatte texture
                        )
                        model.letterBlocks
              }
            , Cmd.none
            )

        --
        --
        --
        UserChosePlayWithStranger ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    ( model, Lamdera.sendToBackend PlayWithStranger )

                Waiting _ ->
                    ( model, Cmd.none )

                InGame _ ->
                    ( model, Cmd.none )

        UserChoseHostFriend ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    ( model, Lamdera.sendToBackend HostFriend )

                Waiting _ ->
                    ( model, Cmd.none )

                InGame _ ->
                    ( model, Cmd.none )

        UserChangedJoinCode joinCode ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    ( { model | page = Home joinCode False }
                    , Cmd.none
                    )

                Waiting _ ->
                    ( model, Cmd.none )

                InGame _ ->
                    ( model, Cmd.none )

        UserChoseJoinFriend ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home joinCode _ ->
                    ( { model | page = Home joinCode False }
                    , Lamdera.sendToBackend (JoinFriend joinCode)
                    )

                Waiting _ ->
                    ( model, Cmd.none )

                InGame _ ->
                    ( model, Cmd.none )

        UserAbandonedWaiting ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    ( model, Cmd.none )

                Waiting _ ->
                    ( { model | page = Home "" False }, Lamdera.sendToBackend AbandonWaiting )

                InGame _ ->
                    ( model, Cmd.none )

        --
        GameMessage gameMsg ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    ( model, Cmd.none )

                Waiting _ ->
                    ( model, Cmd.none )

                InGame gameModel ->
                    updateGame model gameMsg gameModel

        --
        Admin_ClickedClearAllMatches ->
            ( model, Lamdera.sendToBackend Admin_ClearAllMatches )


updateGame : FrontendModel -> GameMsg -> GameFrontend -> ( FrontendModel, Cmd FrontendMsg )
updateGame feModel msg model =
    case msg of
        UserRequestedNewGame ->
            ( { feModel | page = Home "" False }, Cmd.none )

        UserRequestedLeaveMatch ->
            ( { feModel | page = Home "" False }, Lamdera.sendToBackend LeaveMatchRequested )

        Tick delta ->
            Tuple.mapFirst (\gm -> { feModel | page = InGame gm }) <|
                if model.stage == Simulating then
                    let
                        m =
                            Timestep.advance simulateStep delta model
                    in
                    case model.opponentDisconnected of
                        Nothing ->
                            ( m, Cmd.none )

                        Just opponentDisconnected ->
                            ( { m | opponentDisconnected = Just (opponentDisconnected |> Quantity.minus delta) }, Cmd.none )

                else
                    case model.opponentDisconnected of
                        Nothing ->
                            ( model, Cmd.none )

                        Just opponentDisconnected ->
                            ( { model | opponentDisconnected = Just (opponentDisconnected |> Quantity.minus delta) }, Cmd.none )

        UserEnteredElevation angle ->
            ( { model | elevantionRaw = angle }
            , Cmd.none
            )
                |> Tuple.mapFirst (\gm -> { feModel | page = InGame gm })

        UserEnteredRotation angle ->
            ( { model | rotationRaw = angle }
            , Cmd.none
            )
                |> Tuple.mapFirst (\gm -> { feModel | page = InGame gm })

        UserEnteredForce force ->
            ( { model | forceRaw = force }
            , Cmd.none
            )
                |> Tuple.mapFirst (\gm -> { feModel | page = InGame gm })

        UserFiredBall ->
            Tuple.mapFirst (\gm -> { feModel | page = InGame gm }) <|
                if model.myColor == model.turn then
                    case
                        ( String.toFloat model.elevantionRaw
                        , String.toFloat model.rotationRaw
                        , String.toFloat model.forceRaw
                        )
                    of
                        ( Just elevationF, Just rotationF, Just forceF ) ->
                            ( fireBall elevationF rotationF forceF model
                            , Lamdera.sendToBackend (Fire elevationF rotationF forceF)
                            )

                        _ ->
                            ( model, Cmd.none )

                else
                    ( model, Cmd.none )

        UserRotatedCamera cameraRotation ->
            ( { model
                | cameraRotation =
                    cameraRotation
                        |> String.toFloat
                        |> Maybe.withDefault model.cameraRotation
              }
            , Cmd.none
            )
                |> Tuple.mapFirst (\gm -> { feModel | page = InGame gm })


fireBall : Float -> Float -> Float -> GameFrontend -> GameFrontend
fireBall elevationF rotationF forceF model =
    let
        impulse =
            Vector3d.withLength
                (Quantity.times (Duration.seconds 0.005)
                    (Force.meganewtons forceF)
                )
                ((case model.turn of
                    Red ->
                        Direction3d.negativeX

                    Blue ->
                        Direction3d.positiveX
                 )
                    |> Direction3d.rotateAround
                        (case model.turn of
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
    { model
        | stage = Simulating
        , bodies =
            List.map
                (\( id, body ) ->
                    ( id
                    , case id of
                        RedBall _ _ ->
                            case model.turn of
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
                            case model.turn of
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
                model.bodies
    }


simulateStep : GameFrontend -> GameFrontend
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
            case model.stage of
                Aiming ->
                    model.elapsed

                Simulating ->
                    Quantity.plus model.elapsed (Timestep.duration model.timestep)

                GameOver _ ->
                    model.elapsed

        ( blueTowers, redTowers ) =
            countAllTowers simulated

        ( fallenBlueTowers, fallenRedTowers ) =
            countFallenTowers newContacts

        newModel =
            { model
                | bodies = simulated
                , prevBodies = model.bodies
                , contacts = newContacts
                , elapsed = newElapsed
                , redTowersRemaining = redTowers - fallenRedTowers
                , blueTowersRemaining = blueTowers - fallenBlueTowers
            }
    in
    if newModel.redTowersRemaining < 1 then
        { newModel
            | stage = GameOver Blue
        }

    else if newModel.blueTowersRemaining < 1 then
        { newModel
            | stage = GameOver Red
        }

    else if newElapsed |> Quantity.greaterThanOrEqualTo (Duration.seconds 5) then
        { newModel
            | stage = Aiming
            , turn =
                case newModel.turn of
                    Red ->
                        Blue

                    Blue ->
                        Red
            , elapsed = Duration.seconds 0
            , cameraRotation =
                case newModel.turn of
                    Red ->
                        90

                    Blue ->
                        0
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


camera : Float -> Camera3d Meters WorldCoordinates
camera cameraRotation =
    Camera3d.lookAt
        { eyePoint =
            eyePoint
                |> Point3d.rotateAround Axis3d.z
                    (Angle.degrees cameraRotation)
        , focalPoint = Point3d.meters 0 0 3
        , upDirection = Direction3d.positiveZ
        , projection = Camera3d.Perspective
        , fov = Camera3d.angle (Angle.degrees 24)
        }


eyePoint : Point3d Meters WorldCoordinates
eyePoint =
    Point3d.meters 40 40 20


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        Admin_LoggedIn ->
            ( { model | page = AdminView }
            , Cmd.none
            )

        Admin_ForcedReset ->
            ( { model | page = Home "" False }, Cmd.none )

        --
        BeginWaitingForStranger ->
            ( { model
                | page = Waiting ""
              }
            , Cmd.none
            )

        BeginWaitingForFriend gameId ->
            ( { model
                | page = Waiting ("Share this game code with your friend: " ++ gameId)
              }
            , Cmd.none
            )

        UnknownJoinCode ->
            let
                joinCode =
                    case model.page of
                        Home jc _ ->
                            jc

                        _ ->
                            ""
            in
            ( { model
                | page = Home joinCode True
              }
            , Cmd.none
            )

        GameStarted myColor ->
            let
                ( blueTowers, redTowers ) =
                    countAllTowers initBodies
            in
            ( { model
                | page =
                    InGame
                        { myColor = myColor
                        , opponentDisconnected = Nothing
                        , bodies = initBodies
                        , prevBodies = initBodies
                        , contacts = Physics.emptyContacts
                        , elevantionRaw = ""
                        , rotationRaw = ""
                        , forceRaw = ""
                        , turn = Red
                        , stage = Aiming
                        , elapsed = Duration.seconds 0
                        , timestep = initTimestep
                        , cameraRotation =
                            case myColor of
                                Red ->
                                    0

                                Blue ->
                                    90
                        , redTowersRemaining = redTowers
                        , blueTowersRemaining = blueTowers
                        }
              }
            , Cmd.none
            )

        OtherPlayerLeft ->
            ( { model
                | page = Waiting "Other player left"
              }
            , Cmd.none
            )

        GameRejoined game ->
            ( { model
                | page =
                    InGame
                        { myColor = game.yourColor
                        , opponentDisconnected = Nothing
                        , bodies = game.bodies
                        , prevBodies = game.bodies
                        , contacts = game.contacts
                        , elevantionRaw = ""
                        , rotationRaw = ""
                        , forceRaw = ""
                        , turn = game.turn
                        , stage = game.stage
                        , elapsed = game.elapsed
                        , timestep = game.timestep
                        , cameraRotation =
                            case game.yourColor of
                                Red ->
                                    0

                                Blue ->
                                    90
                        , redTowersRemaining = game.redTowersRemaining
                        , blueTowersRemaining = game.blueTowersRemaining
                        }
              }
            , Cmd.none
            )

        TurnChange changes ->
            case model.page of
                InGame game ->
                    ( { model
                        | page =
                            InGame
                                { game
                                    | bodies = changes.bodies
                                    , prevBodies = changes.bodies
                                    , contacts = changes.contacts
                                    , turn = changes.turn
                                    , stage = changes.stage
                                    , elapsed = Duration.seconds 0
                                    , timestep = initTimestep
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    -- [TODO]: We should send a message to the backend that
                    -- the other person might be stuck in the game stil
                    ( model, Cmd.none )

        OtherPlayerFired elevationF rotationF forceF ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    -- [TODO]
                    ( model, Cmd.none )

                Waiting _ ->
                    -- [TODO]
                    ( model, Cmd.none )

                InGame game ->
                    ( { model | page = InGame (fireBall elevationF rotationF forceF game) }
                    , Cmd.none
                    )

        OpponentDisconnected ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    ( model, Cmd.none )

                Waiting _ ->
                    ( model, Cmd.none )

                InGame game ->
                    ( { model | page = InGame { game | opponentDisconnected = Just (Duration.seconds 30) } }, Cmd.none )

        OpponentConnected ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    ( model, Cmd.none )

                Waiting _ ->
                    ( model, Cmd.none )

                InGame game ->
                    ( { model | page = InGame { game | opponentDisconnected = Nothing } }, Cmd.none )

        OpponentLeft ->
            case model.page of
                AdminView ->
                    ( model, Cmd.none )

                Home _ _ ->
                    ( model, Cmd.none )

                Waiting _ ->
                    ( model, Cmd.none )

                InGame game ->
                    -- TODO: Add a message of some kind here
                    ( { model | page = Home "" False }, Cmd.none )


initTimestep : Timestep
initTimestep =
    Timestep.init
        { duration = Duration.seconds (1 / 120)
        , maxSteps = 2
        }


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Block Topple"
    , body =
        case model.page of
            AdminView ->
                viewAdmin model

            Home gameToJoin joinError ->
                viewHome model gameToJoin joinError

            Waiting additionalMessage ->
                viewWaiting additionalMessage

            InGame gameModel ->
                viewGame model gameModel
    }


viewAdmin : FrontendModel -> List (Html FrontendMsg)
viewAdmin model =
    [ Html.h1 [] [ Html.text "Block Topple - Admin" ]
    , Html.button
        [ Html.Events.onClick Admin_ClickedClearAllMatches
        ]
        [ Html.text "Clear all matches" ]
    ]


viewHome : FrontendModel -> String -> Bool -> List (Html FrontendMsg)
viewHome model gameToJoin joinError =
    [ Html.div
        [ Css.home ]
        [ Html.h1 [] [ Html.text "Block Topple" ]
        , Scene3d.sunny
            { upDirection = Direction3d.positiveZ
            , sunlightDirection =
                Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -120)
            , shadows = True
            , camera =
                Camera3d.lookAt
                    { eyePoint = Point3d.meters 0 14 1
                    , focalPoint = Point3d.meters 0 0 0
                    , upDirection = Direction3d.positiveZ
                    , projection = Camera3d.Perspective
                    , fov = Camera3d.angle (Angle.degrees 24)
                    }
            , dimensions = ( Pixels.int 800, Pixels.int 250 )
            , background =
                -- Scene3d.backgroundColor (Color.rgb255 100 149 237)
                Scene3d.transparentBackground
            , clipDepth = Length.meters 0.1
            , entities =
                List.concat
                    [ stringToBlocks model "ab"
                        |> List.map (Scene3d.translateBy (Vector3d.meters 5 5 0))
                    , case model.cylinderMesh of
                        Nothing ->
                            []

                        Just ( mesh, meshShadow ) ->
                            [ Scene3d.meshWithShadow
                                (Scene3d.Material.nonmetal
                                    { baseColor = Color.blue
                                    , roughness = 0.25
                                    }
                                )
                                mesh
                                meshShadow
                                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 6 -1))
                            , Scene3d.meshWithShadow
                                (Scene3d.Material.nonmetal
                                    { baseColor = Color.blue
                                    , roughness = 0.25
                                    }
                                )
                                mesh
                                meshShadow
                                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 6 0))
                            ]
                    , let
                        cone =
                            Cone3d.startingAt
                                Point3d.origin
                                Direction3d.positiveZ
                                { radius = Length.centimeters 60
                                , length = Length.centimeters 80
                                }
                      in
                      [ Scene3d.coneWithShadow
                            (Scene3d.Material.nonmetal
                                { baseColor = Color.blue
                                , roughness = 0.25
                                }
                            )
                            cone
                            |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 6 1))
                      ]
                    ]
            }
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick UserChosePlayWithStranger
            ]
            [ Html.text "Play with a stranger" ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick UserChoseHostFriend
            ]
            [ Html.text "Host a friend" ]
        , Html.form
            [ Css.joinForm
            , Html.Events.onSubmit UserChoseJoinFriend
            ]
            [ Html.label
                []
                [ Html.span [] [ Html.text "Game to join" ]
                , Html.input
                    [ Html.Attributes.value gameToJoin
                    , Html.Events.onInput UserChangedJoinCode
                    ]
                    []
                ]
            , Html.button
                [ Html.Attributes.type_ "submit" ]
                [ Html.text "Join a friend" ]
            ]
        , if joinError then
            Html.p []
                [ Html.text "I wasn't able to find that game. Pleae double check that your friend gave you the right code, or try having them get a new code." ]

          else
            Html.text ""
        ]
    ]


stringToBlocks : FrontendModel -> String -> List (Scene3d.Entity ())
stringToBlocks model str =
    String.foldl
        (\letter ( entities, offset ) ->
            ( case Dict.get letter model.letterBlocks |> Debug.log ("letter " ++ String.fromChar letter ++ "?") of
                Nothing ->
                    entities

                Just ( ( mesh, meshShadow ), material ) ->
                    (Scene3d.meshWithShadow
                        material
                        mesh
                        meshShadow
                        |> Scene3d.placeIn Frame3d.atOrigin
                        |> Scene3d.translateBy (Vector3d.meters offset 0 0)
                    )
                        :: entities
            , offset - 1
            )
        )
        ( [], 0 )
        str
        |> Tuple.first
        |> List.reverse


viewWaiting : String -> List (Html FrontendMsg)
viewWaiting additionalMessage =
    [ Html.div
        [ Css.waiting
        ]
        [ Html.p [] [ Html.text additionalMessage ]
        , Html.p [] [ Html.text "waiting for another player ..." ]
        , Html.button
            [ Html.Attributes.type_ "button"
            , Html.Events.onClick UserAbandonedWaiting
            ]
            [ Html.text "Main menu" ]
        ]
    ]


viewGame : FrontendModel -> GameFrontend -> List (Html FrontendMsg)
viewGame model gameModel =
    List.map (Html.map GameMessage) <|
        [ Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "top" "0"
            ]
            [ Scene3d.sunny
                { upDirection = Direction3d.positiveZ
                , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
                , shadows = True
                , camera = camera gameModel.cameraRotation
                , dimensions = model.dimensions
                , background = Scene3d.backgroundColor (Color.rgb255 100 149 237)
                , clipDepth = Length.meters 0.1
                , entities =
                    let
                        ballCenter =
                            listFindMap
                                (\( id, body ) ->
                                    case id of
                                        RedBall _ _ ->
                                            case gameModel.turn of
                                                Red ->
                                                    Physics.centerOfMass body

                                                Blue ->
                                                    Nothing

                                        BlueBall _ _ ->
                                            case gameModel.turn of
                                                Blue ->
                                                    Physics.centerOfMass body

                                                Red ->
                                                    Nothing

                                        _ ->
                                            Nothing
                                )
                                gameModel.bodies
                                |> Maybe.withDefault Point3d.origin
                    in
                    (case gameModel.stage of
                        Aiming ->
                            let
                                arrowDirection =
                                    (case gameModel.turn of
                                        Red ->
                                            Direction3d.negativeX

                                        Blue ->
                                            Direction3d.positiveX
                                    )
                                        |> Direction3d.rotateAround
                                            (case gameModel.turn of
                                                Red ->
                                                    Direction3d.positiveY

                                                Blue ->
                                                    Direction3d.negativeY
                                            )
                                            (gameModel.elevantionRaw
                                                |> String.toFloat
                                                |> Maybe.withDefault 0
                                                |> Angle.degrees
                                            )
                                        |> Direction3d.rotateAround
                                            Direction3d.negativeZ
                                            (gameModel.rotationRaw
                                                |> String.toFloat
                                                |> Maybe.withDefault 0
                                                |> Angle.degrees
                                            )

                                arrowLength =
                                    gameModel.forceRaw
                                        |> String.toFloat
                                        |> Maybe.map (\f -> f * 8)
                                        |> Maybe.withDefault 100
                                        |> Length.centimeters
                            in
                            Scene3d.group
                                [ Scene3d.cylinder
                                    (Scene3d.Material.matte Color.green)
                                    (Cylinder3d.startingAt
                                        ballCenter
                                        arrowDirection
                                        { radius = Length.centimeters 5
                                        , length = arrowLength
                                        }
                                    )
                                , Scene3d.cone
                                    (Scene3d.Material.matte Color.green)
                                    (Cone3d.startingAt
                                        (ballCenter
                                            |> Point3d.translateIn arrowDirection
                                                arrowLength
                                        )
                                        arrowDirection
                                        { radius = Length.centimeters 15
                                        , length = Length.centimeters 40
                                        }
                                    )
                                ]

                        Simulating ->
                            Scene3d.nothing

                        GameOver _ ->
                            Scene3d.nothing
                    )
                        :: backgroundScenery
                        :: List.map
                            (bodyToEntity
                                model.boxMesh
                                model.boxMaterialRed
                                model.boxMaterialBlue
                                model.cylinderMesh
                            )
                            gameModel.bodies
                }
            ]
        , case gameModel.stage of
            GameOver winner ->
                Html.div
                    [ Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "top" "30%"
                    , Html.Attributes.style "width" "100vw"
                    , Html.Attributes.style "text-align" "center"
                    ]
                    [ Html.h1
                        [ Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.75)"
                        , Html.Attributes.style "color" "white"
                        , Html.Attributes.style "width" "100vw"
                        ]
                        [ Html.text <|
                            if gameModel.myColor == winner then
                                "You won!"

                            else
                                "They won"
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Attributes.style "font-size" "1.25rem"
                        , Html.Attributes.style "border" "3px solid white"
                        , Html.Attributes.style "border-radius" "0.5rem"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "color" "white"
                        , Html.Attributes.style "background-color" "cornflowerblue"
                        , Html.Events.onClick UserRequestedNewGame
                        ]
                        [ Html.text "New Game" ]
                    ]

            _ ->
                Html.form
                    [ Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "gap" "0.5rem"
                    , Html.Events.onSubmit UserFiredBall
                    , Html.Attributes.disabled (gameModel.stage /= Aiming)
                    ]
                    [ Html.input
                        [ Html.Attributes.placeholder "Elevation (Degrees)"
                        , Html.Attributes.style "font-size" "1.25rem"
                        , Html.Attributes.type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value gameModel.elevantionRaw
                        , Html.Events.onInput UserEnteredElevation
                        ]
                        []
                    , Html.input
                        [ Html.Attributes.placeholder "Rotation (Degrees)"
                        , Html.Attributes.style "font-size" "1.25rem"
                        , Html.Attributes.type_ "number"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value gameModel.rotationRaw
                        , Html.Events.onInput UserEnteredRotation
                        ]
                        []
                    , Html.input
                        [ Html.Attributes.placeholder "Force (Meganewtons)"
                        , Html.Attributes.style "font-size" "1.25rem"
                        , Html.Attributes.type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.step "1"
                        , Html.Attributes.max "80"
                        , Html.Attributes.value gameModel.forceRaw
                        , Html.Events.onInput UserEnteredForce
                        ]
                        []
                    , Html.button
                        [ Html.Attributes.type_ "submit"
                        , Html.Attributes.style "font-size" "1.25rem"
                        , Html.Attributes.style "border-width" "2px"
                        , Html.Attributes.style "border-style" "solid"
                        , Html.Attributes.style "border-color" <|
                            if gameModel.turn == gameModel.myColor then
                                "white"

                            else
                                "rgba(0, 0, 0, 0.75)"
                        , Html.Attributes.style "border-radius" "0.5rem"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "color" "white"
                        , Html.Attributes.style "background-color" <|
                            if gameModel.turn == gameModel.myColor then
                                turnToCssColor gameModel.myColor

                            else
                                "rgba(0, 0, 0, 0.75)"
                        ]
                        [ Html.text <|
                            if gameModel.turn == gameModel.myColor then
                                "Fire!"

                            else
                                "Wait"
                        ]
                    , Html.p
                        [ Html.Attributes.style "color" "white"
                        , Html.Attributes.style "background-color" "rgba(70, 70, 70, 0.75)"
                        , Html.Attributes.style "padding" "0.25rem"
                        , Html.Attributes.style "text-align" "center"
                        , Html.Attributes.style "margin" "0"
                        ]
                        [ Html.text <|
                            if gameModel.myColor == gameModel.turn then
                                "Your turn"

                            else
                                "Their turn"
                        ]
                    , Html.p
                        [ Html.Attributes.style "color" "white"
                        , Html.Attributes.style "background-color" "rgba(70, 70, 70, 0.75)"
                        , Html.Attributes.style "padding" "0.25rem"
                        , Html.Attributes.style "text-align" "center"
                        , Html.Attributes.style "margin" "0"
                        ]
                        [ Html.text <|
                            ("Towers remaining: "
                                ++ (String.fromInt <|
                                        if gameModel.myColor == Red then
                                            gameModel.redTowersRemaining

                                        else
                                            gameModel.blueTowersRemaining
                                   )
                            )
                        ]
                    , case gameModel.opponentDisconnected of
                        Nothing ->
                            Html.text ""

                        Just timeToReconnect ->
                            Html.p
                                [ Html.Attributes.style "color" "white"
                                , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.75)"
                                , Html.Attributes.style "padding" "0.25rem"
                                , Html.Attributes.style "text-align" "center"
                                , Html.Attributes.style "margin" "0"
                                ]
                                [ timeToReconnect
                                    |> Duration.inSeconds
                                    |> floor
                                    |> String.fromInt
                                    |> (\s -> "Opponent disconnect: " ++ s ++ "s")
                                    |> Html.text
                                ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Attributes.style "font-size" "1.25rem"
                        , Html.Attributes.style "border" "2px solid white"
                        , Html.Attributes.style "border-radius" "0.5rem"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "color" "white"
                        , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.75)"
                        , Html.Events.onClick UserRequestedLeaveMatch
                        ]
                        [ Html.text "Leave match"
                        ]
                    ]
        , Html.input
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "bottom" "2rem"
            , Html.Attributes.type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "360"
            , Html.Attributes.step "1"
            , Html.Attributes.value (String.fromFloat gameModel.cameraRotation)
            , Html.Events.onInput UserRotatedCamera
            ]
            []
        ]


turnToCssColor turn =
    case turn of
        Red ->
            "red"

        Blue ->
            "blue"


backgroundScenery : Scene3d.Entity WorldCoordinates
backgroundScenery =
    Scene3d.group
        [ Scene3d.sphere
            (Scene3d.Material.matte Color.darkGreen)
            (Sphere3d.atPoint
                (Point3d.centimeters 0 -4000 -400)
                (Length.centimeters 1000)
            )
        , Scene3d.sphere
            (Scene3d.Material.matte Color.darkGreen)
            (Sphere3d.atPoint
                (Point3d.centimeters 900 -4000 -400)
                (Length.centimeters 800)
            )
        , Scene3d.sphere
            (Scene3d.Material.matte Color.darkGreen)
            (Sphere3d.atPoint
                (Point3d.centimeters 700 -3200 -400)
                (Length.centimeters 600)
            )

        --
        , Scene3d.block
            (Scene3d.Material.matte Color.darkGray)
            (Block3d.centeredOn
                (Frame3d.atPoint
                    (Point3d.centimeters 200 4700 0)
                    |> Frame3d.rotateAroundOwn Frame3d.xAxis (Angle.degrees 45)
                    |> Frame3d.rotateAroundOwn Frame3d.yAxis (Angle.degrees 45)
                )
                ( Length.centimeters 1200
                , Length.centimeters 1200
                , Length.centimeters 1200
                )
            )
        , Scene3d.block
            (Scene3d.Material.matte Color.darkGray)
            (Block3d.centeredOn
                (Frame3d.atPoint
                    (Point3d.centimeters 1000 4700 -150)
                    |> Frame3d.rotateAroundOwn Frame3d.xAxis (Angle.degrees 40)
                    |> Frame3d.rotateAroundOwn Frame3d.yAxis (Angle.degrees 47)
                )
                ( Length.centimeters 700
                , Length.centimeters 700
                , Length.centimeters 700
                )
            )
        , Scene3d.block
            (Scene3d.Material.matte Color.darkGray)
            (Block3d.centeredOn
                (Frame3d.atPoint
                    (Point3d.centimeters 300 3200 -100)
                    |> Frame3d.rotateAroundOwn Frame3d.xAxis (Angle.degrees 49)
                    |> Frame3d.rotateAroundOwn Frame3d.yAxis (Angle.degrees 38)
                    |> Frame3d.rotateAroundOwn Frame3d.zAxis (Angle.degrees -38)
                )
                ( Length.centimeters 700
                , Length.centimeters 700
                , Length.centimeters 700
                )
            )
        , Scene3d.block
            (Scene3d.Material.matte Color.darkGray)
            (Block3d.centeredOn
                (Frame3d.atPoint
                    (Point3d.centimeters -800 4200 -100)
                    |> Frame3d.rotateAroundOwn Frame3d.xAxis (Angle.degrees 49)
                    |> Frame3d.rotateAroundOwn Frame3d.yAxis (Angle.degrees -38)
                    |> Frame3d.rotateAroundOwn Frame3d.zAxis (Angle.degrees -38)
                )
                ( Length.centimeters 700
                , Length.centimeters 700
                , Length.centimeters 700
                )
            )
        ]


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


bodyToEntity :
    Maybe CustomMesh
    -> Maybe (Scene3d.Material.Textured BodyCoordinates)
    -> Maybe (Scene3d.Material.Textured BodyCoordinates)
    -> Maybe CustomMesh
    -> ( Id, Body )
    -> Entity WorldCoordinates
bodyToEntity boxMesh boxMaterialRed boxMaterialBlue cylinderMesh ( id, body ) =
    Scene3d.placeIn (Physics.frame body) <|
        case id of
            Floor ->
                Scene3d.quad (Scene3d.Material.matte Color.darkCharcoal)
                    (Point3d.meters -90 -90 0)
                    (Point3d.meters -90 90 0)
                    (Point3d.meters 90 90 0)
                    (Point3d.meters 90 -90 0)

            Block (Box s) c ->
                let
                    fallbackMaterial =
                        Scene3d.Material.nonmetal
                            { baseColor = c
                            , roughness = 0.25
                            }

                    material =
                        if Color.lightRed == c then
                            case boxMaterialRed of
                                Nothing ->
                                    fallbackMaterial

                                Just mat ->
                                    mat

                        else
                            case boxMaterialBlue of
                                Nothing ->
                                    fallbackMaterial

                                Just mat ->
                                    mat
                in
                case boxMesh of
                    Nothing ->
                        Scene3d.blockWithShadow
                            fallbackMaterial
                            s

                    Just ( mesh, meshShadow ) ->
                        Scene3d.meshWithShadow
                            material
                            mesh
                            meshShadow
                            |> Scene3d.translateBy (Vector3d.from (Point3d.meters 0 0 0.5) (Block3d.centerPoint s))

            Block (Cylinder s) c ->
                case cylinderMesh of
                    Nothing ->
                        Scene3d.cylinderWithShadow
                            (Scene3d.Material.nonmetal
                                { baseColor = c
                                , roughness = 0.25
                                }
                            )
                            s

                    Just ( mesh, meshShadow ) ->
                        Scene3d.meshWithShadow
                            (Scene3d.Material.nonmetal
                                { baseColor = c
                                , roughness = 0.25
                                }
                            )
                            mesh
                            meshShadow
                            |> Scene3d.translateBy (Vector3d.from (Point3d.meters 0 0 0.5) (Cylinder3d.centerPoint s))

            Block (Cone s) c ->
                Scene3d.coneWithShadow
                    (Scene3d.Material.nonmetal
                        { baseColor = c
                        , roughness = 0.25
                        }
                    )
                    s

            RedBall b c ->
                Scene3d.sphereWithShadow
                    (Scene3d.Material.nonmetal
                        { baseColor = c
                        , roughness = 0.25
                        }
                    )
                    b

            BlueBall b c ->
                Scene3d.sphereWithShadow
                    (Scene3d.Material.nonmetal
                        { baseColor = c
                        , roughness = 0.25
                        }
                    )
                    b
