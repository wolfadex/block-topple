module Frontend exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
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
import Url
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


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , page = Waiting ""
      , dimensions = ( Pixels.int 0, Pixels.int 0 )

      --
      , boxMesh = Nothing
      , boxMaterialRed = Nothing
      , boxMaterialBlue = Nothing
      , cylinderMesh = Nothing
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


noCmd : model -> ( model, Cmd FrontendMsg )
noCmd model =
    ( model, Cmd.none )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        Resize width height ->
            { model | dimensions = ( Pixels.int width, Pixels.int height ) }
                |> noCmd

        BoxMeshLoaded (Err err) ->
            Debug.todo (Debug.toString err)

        BoxMeshLoaded (Ok boxMesh) ->
            { model | boxMesh = Just ( boxMesh, Scene3d.Mesh.shadow boxMesh ) }
                |> noCmd

        BoxRedTextureLoaded (Err err) ->
            Debug.todo (Debug.toString err)

        BoxRedTextureLoaded (Ok texture) ->
            { model | boxMaterialRed = Just (Scene3d.Material.texturedMatte texture) }
                |> noCmd

        BoxBlueTextureLoaded (Err err) ->
            Debug.todo (Debug.toString err)

        BoxBlueTextureLoaded (Ok texture) ->
            { model | boxMaterialBlue = Just (Scene3d.Material.texturedMatte texture) }
                |> noCmd

        CylinderMeshLoaded (Err err) ->
            Debug.todo (Debug.toString err)

        CylinderMeshLoaded (Ok cylinderMesh) ->
            { model | cylinderMesh = Just ( cylinderMesh, Scene3d.Mesh.shadow cylinderMesh ) }
                |> noCmd

        GameMessage gameMsg ->
            case model.page of
                Waiting _ ->
                    ( model, Cmd.none )

                InGame gameModel ->
                    updateGame gameMsg gameModel
                        |> Tuple.mapFirst (\gm -> { model | page = InGame gm })


updateGame : GameMsg -> GameFrontend -> ( GameFrontend, Cmd FrontendMsg )
updateGame msg model =
    case msg of
        Tick delta ->
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
            { model | elevantionRaw = angle }
                |> noCmd

        UserEnteredRotation angle ->
            { model | rotationRaw = angle }
                |> noCmd

        UserEnteredForce force ->
            { model | forceRaw = force }
                |> noCmd

        UserFiredBall ->
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
            { model
                | cameraRotation =
                    cameraRotation
                        |> String.toFloat
                        |> Maybe.withDefault model.cameraRotation
            }
                |> noCmd


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
                Waiting _ ->
                    Debug.todo ""

                InGame game ->
                    ( { model
                        | page =
                            InGame
                                { game
                                    | bodies = changes.bodies
                                    , prevBodies = changes.bodies
                                    , contacts = changes.contacts
                                    , turn = changes.turn
                                    , stage = Aiming
                                    , elapsed = Duration.seconds 0
                                    , timestep = initTimestep
                                }
                      }
                    , Cmd.none
                    )

        OtherPlayerFired elevationF rotationF forceF ->
            case model.page of
                Waiting _ ->
                    Debug.todo ""

                InGame game ->
                    ( { model | page = InGame (fireBall elevationF rotationF forceF game) }
                    , Cmd.none
                    )

        OpponentDisconnected ->
            case model.page of
                Waiting _ ->
                    ( model, Cmd.none )

                InGame game ->
                    ( { model | page = InGame { game | opponentDisconnected = Just (Duration.seconds 30) } }, Cmd.none )

        OpponentConnected ->
            case model.page of
                Waiting _ ->
                    ( model, Cmd.none )

                InGame game ->
                    ( { model | page = InGame { game | opponentDisconnected = Nothing } }, Cmd.none )


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
            Waiting additionalMessage ->
                [ Html.div
                    [ Html.Attributes.style "position" "fixed"
                    , Html.Attributes.style "left" "50%"
                    , Html.Attributes.style "top" "50%"
                    , Html.Attributes.style "transform" "translate(-50%, -50%)"
                    ]
                    [ Html.p [] [ Html.text additionalMessage ]
                    , Html.p [] [ Html.text "waiting for another player ..." ]
                    ]
                ]

            InGame gameModel ->
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
                                    , Html.Attributes.style "border" "3px solid white"
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
                                    , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.75)"
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
                                    , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.75)"
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
    }


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
