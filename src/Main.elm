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
import Json.Decode exposing (Decoder)
import Length exposing (Length, Meters)
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
import Scene3d exposing (Entity, backgroundColor)
import Scene3d.Material as Material
import Sphere3d exposing (Sphere3d)
import Task
import Timestep exposing (Timestep)
import Vector3d


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
    }


type Turn
    = Red
    | Blue


type Stage
    = Aiming
    | Simulating


type Msg
    = Tick Float
    | Resize Int Int
    | MouseDown (Axis3d Meters WorldCoordinates)
    | MouseMove (Axis3d Meters WorldCoordinates)
    | MouseUp
    | UserEnteredElevation String
    | UserEnteredRotation String
    | UserEnteredForce String
    | UserFiredBall


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


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
      }
    , Task.perform
        (\{ viewport } -> Resize (round viewport.width) (round viewport.height))
        Browser.Dom.getViewport
    )


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
                                    (Vector3d.centimeters -1500 0 0)

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
                                    (Vector3d.centimeters 500 0 0)

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
    , initCylinder
        (Point3d.centimeters xOffset yOffset 300)
        color
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
    Point3d.centimeters 900 130 50


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
    Point3d.centimeters -900 130 50


ballRadius : Length
ballRadius =
    Length.centimeters 60


update : Msg -> Model -> Model
update msg model =
    case msg of
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
                            Plane3d.through dragPoint (Camera3d.viewDirection (camera model.turn))
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

        Resize width height ->
            { model | dimensions = ( Pixels.int width, Pixels.int height ) }

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


camera : Turn -> Camera3d Meters WorldCoordinates
camera turn =
    Camera3d.lookAt
        { eyePoint =
            case turn of
                Red ->
                    Point3d.meters 30 40 20

                Blue ->
                    Point3d.meters -30 40 20
        , focalPoint =
            case turn of
                Red ->
                    Point3d.meters -0.5 -0.5 0

                Blue ->
                    Point3d.meters 0.5 -0.5 0
        , upDirection = Direction3d.positiveZ
        , projection = Camera3d.Perspective
        , fov = Camera3d.angle (Angle.degrees 24)
        }


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
            , Html.Events.on "mousedown" (decodeMouseRay model.turn model.dimensions MouseDown)
            , Html.Events.on "mousemove" (decodeMouseRay model.turn model.dimensions MouseMove)
            , Html.Events.onMouseUp MouseUp
            ]
            [ Scene3d.sunny
                { upDirection = Direction3d.positiveZ
                , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
                , shadows = True
                , camera = camera model.turn
                , dimensions = model.dimensions
                , background = Scene3d.transparentBackground
                , clipDepth = Length.meters 0.1
                , entities =
                    let
                        mouseEntity =
                            case model.dragTarget of
                                Just ( _, dragPoint ) ->
                                    Scene3d.sphere (Material.matte Color.white)
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
                                        |> Maybe.map (\f -> f / 8)
                                        |> Maybe.withDefault 50
                                        |> max 50
                                        |> Length.centimeters
                            in
                            Scene3d.group
                                [ Scene3d.cylinder
                                    (Material.matte Color.green)
                                    (Cylinder3d.startingAt
                                        ballCenter
                                        arrowDirection
                                        { radius = Length.centimeters 5
                                        , length = arrowLength
                                        }
                                    )
                                , Scene3d.cone
                                    (Material.matte Color.green)
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
                        :: mouseEntity
                        :: List.map bodyEntity model.bodies
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
                [ Html.Attributes.placeholder "Force (Newtons)"
                , Html.Attributes.style "font-size" "1.25rem"
                , Html.Attributes.type_ "number"
                , Html.Attributes.min "0"
                , Html.Attributes.step "1"
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
        ]
    }


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


bodyEntity : ( Id, Body ) -> Entity WorldCoordinates
bodyEntity ( id, body ) =
    Scene3d.placeIn (Physics.frame body) <|
        case id of
            Mouse ->
                -- Only used in simulation
                Scene3d.nothing

            Table ->
                Scene3d.group <|
                    List.map
                        (Scene3d.blockWithShadow
                            (Material.nonmetal
                                { baseColor = Color.white
                                , roughness = 0.25
                                }
                            )
                        )
                        tableBlocks

            Floor ->
                Scene3d.quad (Material.matte Color.darkCharcoal)
                    (Point3d.meters -90 -90 0)
                    (Point3d.meters -90 90 0)
                    (Point3d.meters 90 90 0)
                    (Point3d.meters 90 -90 0)

            Block (Box s) c ->
                Scene3d.blockWithShadow
                    (Material.nonmetal
                        { baseColor = c
                        , roughness = 0.25
                        }
                    )
                    s

            Block (Cylinder s) c ->
                Scene3d.cylinderWithShadow
                    (Material.nonmetal
                        { baseColor = c
                        , roughness = 0.25
                        }
                    )
                    s

            RedBall b c ->
                Scene3d.sphereWithShadow
                    (Material.nonmetal
                        { baseColor = c
                        , roughness = 0.25
                        }
                    )
                    b

            BlueBall b c ->
                Scene3d.sphereWithShadow
                    (Material.nonmetal
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
    Turn
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decoder msg
decodeMouseRay turn ( width, height ) rayToMsg =
    Json.Decode.map2
        (\x y ->
            rayToMsg <|
                Camera3d.ray (camera turn)
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
