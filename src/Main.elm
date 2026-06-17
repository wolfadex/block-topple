module Main exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
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
import Vector3d exposing (Vector3d)
import WebGL.Texture


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


type Id
    = Mouse
    | Floor
    | Table
    | Block BlockType Color
    | RedBall (Sphere3d Meters BodyCoordinates) Color
    | BlueBall (Sphere3d Meters BodyCoordinates) Color


type BlockType
    = Box (Block3d Meters BodyCoordinates)
    | Cylinder (Cylinder3d Meters BodyCoordinates)
    | Cone (Cone3d Meters BodyCoordinates)


type alias Model =
    { bodies : List ( Id, Body )
    , prevBodies : List ( Id, Body )
    , contacts : Physics.Contacts Id
    , dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , dragTarget : Maybe ( Point3d Meters BodyCoordinates, Point3d Meters WorldCoordinates )
    , elevantionRaw : String
    , rotationRaw : String
    , forceRaw : String
    , turn : Turn
    , stage : Stage
    , elapsed : Duration
    , timestep : Timestep
    , cameraRotation : Float

    --
    , boxMesh : Maybe CustomMesh
    , boxMaterialRed : Maybe (Scene3d.Material.Textured BodyCoordinates)
    , boxMaterialBlue : Maybe (Scene3d.Material.Textured BodyCoordinates)
    , cylinderMesh : Maybe CustomMesh
    }


type alias CustomMesh =
    ( Scene3d.Mesh.Textured BodyCoordinates
    , Scene3d.Mesh.Shadow BodyCoordinates
    )


type Turn
    = Red
    | Blue


type Stage
    = Aiming
    | Simulating


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bodies = tableOnFloor
      , prevBodies = tableOnFloor
      , contacts = Physics.emptyContacts
      , dimensions = ( Pixels.int 0, Pixels.int 0 )
      , dragTarget = Nothing
      , elevantionRaw = ""
      , rotationRaw = ""
      , forceRaw = ""
      , turn = Red
      , stage = Aiming
      , elapsed = Duration.seconds 0
      , timestep =
            Timestep.init
                { duration = Duration.seconds (1 / 120)
                , maxSteps = 2
                }
      , cameraRotation = 0

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


loadBox : Cmd Msg
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


loadCylinder : Cmd Msg
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


tableBlocks : List (Block3d Meters BodyCoordinates)
tableBlocks =
    [ Block3d.from
        (Point3d.millimeters 222 222 0)
        (Point3d.millimeters 272 272 400)
    , Block3d.from
        (Point3d.millimeters -272 222 0)
        (Point3d.millimeters -222 272 400)
    , Block3d.from
        (Point3d.millimeters -272 -272 0)
        (Point3d.millimeters -222 -222 400)
    , Block3d.from
        (Point3d.millimeters 222 -272 0)
        (Point3d.millimeters 272 -222 400)
    , Block3d.from
        (Point3d.millimeters -275 -275 400)
        (Point3d.millimeters 275 275 450)
    ]


tableOnFloor : List ( Id, Body )
tableOnFloor =
    List.concat
        [ [ ( Floor, Physics.plane Plane3d.xy Physics.Material.wood )
          , initRedBall
          ]
        , initBlockStack Color.lightBlue
            |> List.map
                (\( id, body ) ->
                    ( id
                    , case id of
                        Block _ _ ->
                            body
                                |> Physics.translateBy
                                    (Vector3d.centimeters -1400 0 0)

                        _ ->
                            body
                    )
                )
        , initBlockStack Color.lightRed
            |> List.map
                (\( id, body ) ->
                    ( id
                    , case id of
                        Block _ _ ->
                            body
                                |> Physics.translateBy
                                    (Vector3d.centimeters 600 0 0)

                        _ ->
                            body
                    )
                )
        ]


initBlockStack : Color -> List ( Id, Physics.Body )
initBlockStack color =
    List.concat
        [ initBlockRow 0 -300 50 9 color
        , initBlockRow 0 -150 150 6 color
        , [ initBox
                (Point3d.centimeters 0 -100 250)
                color
          , initBox
                (Point3d.centimeters 0 100 250)
                color
          , initBox
                (Point3d.centimeters 0 300 250)
                color
          ]
        , [ initBox
                (Point3d.centimeters 100 500 50)
                color
          , initBox
                (Point3d.centimeters 200 500 50)
                color
          , initBox
                (Point3d.centimeters 300 500 50)
                color
          , initBox
                (Point3d.centimeters 400 500 50)
                color
          , initBox
                (Point3d.centimeters 500 500 50)
                color
          , initBox
                (Point3d.centimeters 600 500 50)
                color
          , initBox
                (Point3d.centimeters 700 500 50)
                color

          --
          , initBox
                (Point3d.centimeters 150 500 150)
                color
          , initBox
                (Point3d.centimeters 250 500 150)
                color
          , initBox
                (Point3d.centimeters 350 500 150)
                color
          , initBox
                (Point3d.centimeters 450 500 150)
                color
          , initBox
                (Point3d.centimeters 550 500 150)
                color
          , initBox
                (Point3d.centimeters 650 500 150)
                color

          --
          , initBox
                (Point3d.centimeters 200 500 250)
                color
          , initBox
                (Point3d.centimeters 400 500 250)
                color
          , initBox
                (Point3d.centimeters 600 500 250)
                color
          ]
        , initBlockRow 800 -300 50 9 color
        , initBlockRow 800 -150 150 6 color
        , [ initBox
                (Point3d.centimeters 800 -100 250)
                color
          , initBox
                (Point3d.centimeters 800 100 250)
                color
          , initBox
                (Point3d.centimeters 800 300 250)
                color
          ]
        , [ initBox
                (Point3d.centimeters 100 -300 50)
                color
          , initBox
                (Point3d.centimeters 200 -300 50)
                color
          , initBox
                (Point3d.centimeters 300 -300 50)
                color
          , initBox
                (Point3d.centimeters 400 -300 50)
                color
          , initBox
                (Point3d.centimeters 500 -300 50)
                color
          , initBox
                (Point3d.centimeters 600 -300 50)
                color
          , initBox
                (Point3d.centimeters 700 -300 50)
                color

          --
          , initBox
                (Point3d.centimeters 150 -300 150)
                color
          , initBox
                (Point3d.centimeters 250 -300 150)
                color
          , initBox
                (Point3d.centimeters 350 -300 150)
                color
          , initBox
                (Point3d.centimeters 450 -300 150)
                color
          , initBox
                (Point3d.centimeters 550 -300 150)
                color
          , initBox
                (Point3d.centimeters 650 -300 150)
                color

          --
          , initBox
                (Point3d.centimeters 200 -300 250)
                color
          , initBox
                (Point3d.centimeters 400 -300 250)
                color
          , initBox
                (Point3d.centimeters 600 -300 250)
                color
          ]
        , initTower 0 500 color
        , initTower 800 500 color
        , initTower 0 -300 color
        , initTower 800 -300 color
        ]


initTower : Float -> Float -> Color -> List ( Id, Physics.Body )
initTower xOffset yOffset color =
    [ initCylinder
        (Point3d.centimeters xOffset yOffset 100)
        color
    , initCylinder
        (Point3d.centimeters xOffset yOffset 200)
        color
    , let
        cone =
            Cone3d.startingAt
                (Point3d.centimeters xOffset yOffset 300)
                Direction3d.positiveZ
                { radius = Length.centimeters 60
                , length = Length.centimeters 80
                }
      in
      ( Block (Cone cone) Color.gray
      , physicsCone cone
            Physics.Material.wood
      )
    ]


initBlockRow : Float -> Float -> Float -> Int -> Color -> List ( Id, Physics.Body )
initBlockRow xOffset yStart zOffset count color =
    List.range 0 (count - 1)
        |> List.map
            (\index ->
                initBox
                    (Point3d.centimeters xOffset (yStart + toFloat index * 100) zOffset)
                    color
            )


initBox : Point3d Meters BodyCoordinates -> Color -> ( Id, Physics.Body )
initBox center color =
    let
        block =
            Block3d.centeredOn
                (Frame3d.atPoint center)
                ( Length.centimeters 100
                , Length.centimeters 100
                , Length.centimeters 100
                )
    in
    ( Block (Box block) color
    , Physics.block block
        Physics.Material.wood
    )


initCylinder : Point3d Meters BodyCoordinates -> Color -> ( Id, Physics.Body )
initCylinder center color =
    let
        cylinder =
            Cylinder3d.startingAt
                center
                Direction3d.positiveZ
                { radius = Length.centimeters 50
                , length = Length.centimeters 100
                }
    in
    ( Block (Cylinder cylinder) color
    , Physics.cylinder cylinder
        Physics.Material.wood
    )


initRedBall : ( Id, Physics.Body )
initRedBall =
    let
        ball =
            Sphere3d.atPoint
                redBallStart
                ballRadius
    in
    ( RedBall ball Color.lightRed
    , Physics.sphere ball
        Physics.Material.steel
    )


redBallStart : Point3d Meters coordinates
redBallStart =
    Point3d.centimeters 1000 130 50


initBlueBall : ( Id, Physics.Body )
initBlueBall =
    let
        ball =
            Sphere3d.atPoint
                blueBallStart
                ballRadius
    in
    ( BlueBall ball Color.lightBlue
    , Physics.sphere ball
        Physics.Material.steel
    )


blueBallStart : Point3d Meters coordinates
blueBallStart =
    Point3d.centimeters -1000 130 50


ballRadius : Length
ballRadius =
    Length.centimeters 60


type Msg
    = Resize Int Int
    | BoxMeshLoaded (Result Http.Error (Scene3d.Mesh.Textured BodyCoordinates))
    | BoxRedTextureLoaded (Result WebGL.Texture.Error (Scene3d.Material.Texture Color))
    | BoxBlueTextureLoaded (Result WebGL.Texture.Error (Scene3d.Material.Texture Color))
    | CylinderMeshLoaded (Result Http.Error (Scene3d.Mesh.Textured BodyCoordinates))
    | Tick Float
    | MouseDown (Axis3d Meters WorldCoordinates)
    | MouseMove (Axis3d Meters WorldCoordinates)
    | MouseUp
    | UserEnteredElevation String
    | UserEnteredRotation String
    | UserEnteredForce String
    | UserFiredBall
    | UserRotatedCamera String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resize width height ->
            { model | dimensions = ( Pixels.int width, Pixels.int height ) }

        BoxMeshLoaded (Err err) ->
            Debug.todo (Debug.toString err)

        BoxMeshLoaded (Ok boxMesh) ->
            { model | boxMesh = Just ( boxMesh, Scene3d.Mesh.shadow boxMesh ) }

        BoxRedTextureLoaded (Err err) ->
            Debug.todo (Debug.toString err)

        BoxRedTextureLoaded (Ok texture) ->
            { model | boxMaterialRed = Just (Scene3d.Material.texturedMatte texture) }

        BoxBlueTextureLoaded (Err err) ->
            Debug.todo (Debug.toString err)

        BoxBlueTextureLoaded (Ok texture) ->
            { model | boxMaterialBlue = Just (Scene3d.Material.texturedMatte texture) }

        CylinderMeshLoaded (Err err) ->
            Debug.todo (Debug.toString err)

        CylinderMeshLoaded (Ok cylinderMesh) ->
            { model | cylinderMesh = Just ( cylinderMesh, Scene3d.Mesh.shadow cylinderMesh ) }

        Tick delta ->
            case model.dragTarget of
                Just ( pointOnTable, dragPoint ) ->
                    let
                        ( simulated, newContacts ) =
                            Physics.simulate
                                { onEarth
                                    | constrain = lockMouseTo pointOnTable
                                    , contacts = model.contacts

                                    -- , solverIterations = 50
                                    -- , duration = Duration.milliseconds (delta |> Debug.log "delta")
                                }
                                (( Mouse, Physics.static [] |> Physics.moveTo dragPoint )
                                    :: model.bodies
                                )
                    in
                    { model | bodies = List.drop 1 simulated, contacts = newContacts }

                Nothing ->
                    Timestep.advance simulateStep (Duration.milliseconds delta) model

        MouseDown mouseRay ->
            case Physics.raycast mouseRay model.bodies of
                Just ( Table, body, { point } ) ->
                    let
                        pointOnTable =
                            Point3d.relativeTo (Physics.frame body) point
                    in
                    { model | dragTarget = Just ( pointOnTable, point ) }

                _ ->
                    model

        MouseMove mouseRay ->
            case model.dragTarget of
                Just ( pointOnTable, dragPoint ) ->
                    let
                        plane =
                            Plane3d.through dragPoint (Camera3d.viewDirection (camera model.cameraRotation model.turn))
                    in
                    { model
                        | dragTarget =
                            Just
                                ( pointOnTable
                                , Axis3d.intersectionWithPlane plane mouseRay
                                    |> Maybe.withDefault dragPoint
                                )
                    }

                Nothing ->
                    model

        MouseUp ->
            { model | dragTarget = Nothing }

        UserEnteredElevation angle ->
            { model | elevantionRaw = angle }

        UserEnteredRotation angle ->
            { model | rotationRaw = angle }

        UserEnteredForce force ->
            { model | forceRaw = force }

        UserFiredBall ->
            case
                ( String.toFloat model.elevantionRaw
                , String.toFloat model.rotationRaw
                , String.toFloat model.forceRaw
                )
            of
                ( Just elevationF, Just rotationF, Just forceF ) ->
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
                                        (model.rotationRaw
                                            |> String.toFloat
                                            |> Maybe.withDefault 0
                                            |> Angle.degrees
                                        )
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

                _ ->
                    model

        UserRotatedCamera cameraRotation ->
            { model
                | cameraRotation =
                    cameraRotation
                        |> String.toFloat
                        |> Maybe.withDefault model.cameraRotation
            }


simulateStep : Model -> Model
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

        newModel =
            { model
                | bodies = simulated
                , prevBodies = model.bodies
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
            , elevantionRaw = ""
            , rotationRaw = ""
            , forceRaw = ""
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


camera : Float -> Turn -> Camera3d Meters WorldCoordinates
camera cameraRotation turn =
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


lockMouseTo : Point3d Meters BodyCoordinates -> Id -> Maybe (Id -> List Constraint)
lockMouseTo pointOnTable mouseId =
    if mouseId == Mouse then
        Just
            (\tableId ->
                if tableId == Table then
                    [ Physics.Constraint.pointToPoint Point3d.origin pointOnTable ]

                else
                    []
            )

    else
        Nothing


view : Model -> Browser.Document Msg
view model =
    { title = "Block Topple"
    , body =
        [ Html.div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.style "top" "0"
            , Html.Events.on "mousedown" (decodeMouseRay model.cameraRotation model.turn model.dimensions MouseDown)
            , Html.Events.on "mousemove" (decodeMouseRay model.cameraRotation model.turn model.dimensions MouseMove)
            , Html.Events.onMouseUp MouseUp
            ]
            [ Scene3d.sunny
                { upDirection = Direction3d.positiveZ
                , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
                , shadows = True
                , camera = camera model.cameraRotation model.turn
                , dimensions = model.dimensions
                , background = Scene3d.backgroundColor (Color.rgb255 100 149 237)
                , clipDepth = Length.meters 0.1
                , entities =
                    let
                        mouseEntity =
                            case model.dragTarget of
                                Just ( _, dragPoint ) ->
                                    Scene3d.sphere (Scene3d.Material.matte Color.white)
                                        (Sphere3d.atPoint dragPoint (Length.millimeters 20))

                                Nothing ->
                                    Scene3d.nothing

                        ballCenter =
                            listFindMap
                                (\( id, body ) ->
                                    case id of
                                        RedBall _ _ ->
                                            case model.turn of
                                                Red ->
                                                    Physics.centerOfMass body

                                                Blue ->
                                                    Nothing

                                        BlueBall _ _ ->
                                            case model.turn of
                                                Blue ->
                                                    Physics.centerOfMass body

                                                Red ->
                                                    Nothing

                                        _ ->
                                            Nothing
                                )
                                model.bodies
                                |> Maybe.withDefault Point3d.origin
                    in
                    (case model.stage of
                        Aiming ->
                            let
                                arrowDirection =
                                    (case model.turn of
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
                                            (model.elevantionRaw
                                                |> String.toFloat
                                                |> Maybe.withDefault 0
                                                |> Angle.degrees
                                            )
                                        |> Direction3d.rotateAround
                                            Direction3d.negativeZ
                                            (model.rotationRaw
                                                |> String.toFloat
                                                |> Maybe.withDefault 0
                                                |> Angle.degrees
                                            )

                                arrowLength =
                                    model.forceRaw
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
                    )
                        -- :: mouseEntity
                        :: backgroundScenery
                        :: List.map
                            (bodyToEntity
                                model.boxMesh
                                model.boxMaterialRed
                                model.boxMaterialBlue
                                model.cylinderMesh
                            )
                            model.bodies
                }
            ]
        , Html.form
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "gap" "0.5rem"
            , Html.Events.onSubmit UserFiredBall
            , Html.Attributes.disabled (model.stage /= Aiming)
            ]
            [ Html.input
                [ Html.Attributes.placeholder "Elevation (Degrees)"
                , Html.Attributes.style "font-size" "1.25rem"
                , Html.Attributes.type_ "number"
                , Html.Attributes.min "0"
                , Html.Attributes.step "1"
                , Html.Attributes.value model.elevantionRaw
                , Html.Events.onInput UserEnteredElevation
                ]
                []
            , Html.input
                [ Html.Attributes.placeholder "Rotation (Degrees)"
                , Html.Attributes.style "font-size" "1.25rem"
                , Html.Attributes.type_ "number"
                , Html.Attributes.step "1"
                , Html.Attributes.value model.rotationRaw
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
                , Html.Attributes.value model.forceRaw
                , Html.Events.onInput UserEnteredForce
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.style "font-size" "1.25rem"
                , Html.Attributes.style "border" "none"
                , Html.Attributes.style "color" "white"
                , Html.Attributes.style "background-color" <|
                    case model.turn of
                        Red ->
                            "red"

                        Blue ->
                            "blue"
                ]
                [ Html.text "Fire!" ]
            ]
        , Html.input
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "bottom" "2rem"
            , Html.Attributes.type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "360"
            , Html.Attributes.step "1"
            , Html.Attributes.value (String.fromFloat model.cameraRotation)
            , Html.Events.onInput UserRotatedCamera
            ]
            []
        ]
    }


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
            Mouse ->
                -- Only used in simulation
                Scene3d.nothing

            Table ->
                Scene3d.group <|
                    List.map
                        (Scene3d.blockWithShadow
                            (Scene3d.Material.nonmetal
                                { baseColor = Color.white
                                , roughness = 0.25
                                }
                            )
                        )
                        tableBlocks

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resize
        , case model.stage of
            Aiming ->
                Sub.none

            Simulating ->
                Browser.Events.onAnimationFrameDelta Tick
        ]


decodeMouseRay :
    Float
    -> Turn
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decoder msg
decodeMouseRay cameraRotation turn ( width, height ) rayToMsg =
    Json.Decode.map2
        (\x y ->
            rayToMsg <|
                Camera3d.ray (camera cameraRotation turn)
                    (Rectangle2d.with
                        { x1 = Quantity.zero
                        , y1 = Quantity.toFloatQuantity height
                        , x2 = Quantity.toFloatQuantity width
                        , y2 = Quantity.zero
                        }
                    )
                    (Point2d.pixels x y)
        )
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)



--


physicsCone : Cone3d Meters BodyCoordinates -> Physics.Material.Material Physics.Material.Dense -> Body
physicsCone sourceCone material =
    let
        bottomCenter =
            Cone3d.basePoint sourceCone

        tip =
            Cone3d.tipPoint sourceCone

        radius =
            Cone3d.radius sourceCone
                |> Length.inMeters

        bottom =
            TriangularMesh.radial bottomCenter <|
                Parameter1d.leading 12 <|
                    \u ->
                        let
                            theta =
                                2 * pi * u

                            sinTheta =
                                sin theta

                            cosTheta =
                                cos theta
                        in
                        bottomCenter
                            |> Point3d.translateBy
                                (Vector3d.unsafe { x = cosTheta * radius, y = -sinTheta * radius, z = 0 })

        sides =
            TriangularMesh.radial tip <|
                Parameter1d.leading 12 <|
                    \u ->
                        let
                            theta =
                                2 * pi * u

                            sinTheta =
                                sin theta

                            cosTheta =
                                cos theta
                        in
                        bottomCenter
                            |> Point3d.translateBy
                                (Vector3d.unsafe { x = cosTheta * radius, y = sinTheta * radius, z = 0 })
    in
    Physics.dynamic
        [ ( Physics.Shape.unsafeConvex (TriangularMesh.combine [ bottom, sides ])
          , material
          )
        ]
