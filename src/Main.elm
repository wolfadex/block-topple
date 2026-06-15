module Main exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d
import Duration
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
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Sphere3d exposing (Sphere3d)
import Task
import Vector3d


type Id
    = Mouse
    | Floor
    | Table
    | Block (Block3d Meters BodyCoordinates) Color
    | RedBall (Sphere3d Meters BodyCoordinates) Color
    | BlueBall (Sphere3d Meters BodyCoordinates) Color


type alias Model =
    { bodies : List ( Id, Body )
    , contacts : Physics.Contacts Id
    , dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , dragTarget : Maybe ( Point3d Meters BodyCoordinates, Point3d Meters WorldCoordinates )
    , redAngle : Angle
    , redForce : Force
    }


type Msg
    = Tick
    | Resize Int Int
    | MouseDown (Axis3d Meters WorldCoordinates)
    | MouseMove (Axis3d Meters WorldCoordinates)
    | MouseUp
    | UserEnteredRedAngle Angle
    | UserEnteredRedForce Force
    | UserFiredRedBall


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
      , contacts = Physics.emptyContacts
      , dimensions = ( Pixels.int 0, Pixels.int 0 )
      , dragTarget = Nothing
      , redAngle = Angle.degrees 0
      , redForce = Force.newtons 0
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
    [ ( Floor, Physics.plane Plane3d.xy Physics.Material.wood )
    , initBlock
        (Point3d.millimeters -500 0 100)
        Color.lightBlue
    , initBlock
        (Point3d.millimeters -500 105 100)
        Color.lightBlue
    , initBlock
        (Point3d.millimeters -500 210 100)
        Color.lightBlue
    , initBlock
        (Point3d.millimeters -500 315 100)
        Color.lightBlue
    , initBlock
        (Point3d.millimeters -500 50 205)
        Color.lightBlue
    , initBlock
        (Point3d.millimeters -500 155 205)
        Color.lightBlue
    , initBlock
        (Point3d.millimeters -500 260 205)
        Color.lightBlue

    --
    , initBlueBall
        (Point3d.millimeters -900 130 150)
        Color.lightBlue

    --
    , initBlock
        (Point3d.millimeters 500 0 100)
        Color.lightRed
    , initBlock
        (Point3d.millimeters 500 105 100)
        Color.lightRed
    , initBlock
        (Point3d.millimeters 500 210 100)
        Color.lightRed
    , initBlock
        (Point3d.millimeters 500 315 100)
        Color.lightRed
    , initBlock
        (Point3d.millimeters 500 50 205)
        Color.lightRed
    , initBlock
        (Point3d.millimeters 500 155 205)
        Color.lightRed
    , initBlock
        (Point3d.millimeters 500 260 205)
        Color.lightRed

    --
    , initRedBall
        (Point3d.millimeters 900 130 150)
        Color.lightRed
    ]


initBlock : Point3d Meters BodyCoordinates -> Color -> ( Id, Physics.Body )
initBlock center color =
    let
        block =
            Block3d.centeredOn
                (Frame3d.atPoint center)
                ( Length.millimeters 100
                , Length.millimeters 100
                , Length.millimeters 100
                )
    in
    ( Block block color
    , Physics.block block
        Physics.Material.wood
    )


initRedBall : Point3d Meters BodyCoordinates -> Color -> ( Id, Physics.Body )
initRedBall center color =
    let
        ball =
            Sphere3d.atPoint
                center
                ballRadius
    in
    ( RedBall ball color
    , Physics.sphere ball
        Physics.Material.wood
    )


initBlueBall : Point3d Meters BodyCoordinates -> Color -> ( Id, Physics.Body )
initBlueBall center color =
    let
        ball =
            Sphere3d.atPoint
                center
                ballRadius
    in
    ( BlueBall ball color
    , Physics.sphere ball
        Physics.Material.wood
    )


ballRadius : Length
ballRadius =
    Length.millimeters 60


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            case model.dragTarget of
                Just ( pointOnTable, dragPoint ) ->
                    let
                        ( simulated, newContacts ) =
                            Physics.simulate
                                { onEarth | constrain = lockMouseTo pointOnTable, contacts = model.contacts }
                                (( Mouse, Physics.static [] |> Physics.moveTo dragPoint )
                                    :: model.bodies
                                )
                    in
                    { model | bodies = List.drop 1 simulated, contacts = newContacts }

                Nothing ->
                    let
                        ( simulated, newContacts ) =
                            Physics.simulate { onEarth | contacts = model.contacts } model.bodies
                    in
                    { model | bodies = simulated, contacts = newContacts }

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
                            Plane3d.through dragPoint (Camera3d.viewDirection camera)
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

        UserEnteredRedAngle angle ->
            { model | redAngle = angle }

        UserEnteredRedForce force ->
            { model | redForce = force }

        UserFiredRedBall ->
            { model
                | bodies =
                    List.map
                        (\( id, body ) ->
                            ( id
                            , case id of
                                RedBall _ _ ->
                                    let
                                        impulse =
                                            Vector3d.withLength
                                                (Quantity.times (Duration.seconds 0.005)
                                                    model.redForce
                                                )
                                                (Direction3d.negativeX
                                                    |> Direction3d.rotateAround
                                                        Direction3d.negativeY
                                                        model.redAngle
                                                )
                                    in
                                    Physics.applyImpulse
                                        impulse
                                        (Physics.originPoint body
                                            |> Point3d.translateBy
                                                (Vector3d.scaleTo ballRadius impulse)
                                        )
                                        body

                                BlueBall _ _ ->
                                    body

                                _ ->
                                    body
                            )
                        )
                        model.bodies
            }


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.lookAt
        { eyePoint = Point3d.meters 3 4 2
        , focalPoint = Point3d.meters -0.5 -0.5 0
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
            , Html.Events.on "mousedown" (decodeMouseRay model.dimensions MouseDown)
            , Html.Events.on "mousemove" (decodeMouseRay model.dimensions MouseMove)
            , Html.Events.onMouseUp MouseUp
            ]
            [ Scene3d.sunny
                { upDirection = Direction3d.positiveZ
                , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
                , shadows = True
                , camera = camera
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
                    in
                    mouseEntity :: List.map bodyEntity model.bodies
                }
            ]
        , Html.form
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "gap" "0.5rem"
            , Html.Events.onSubmit UserFiredRedBall
            ]
            [ Html.input
                [ Html.Attributes.placeholder "Angle (Degrees)"
                , Html.Attributes.style "font-size" "1.25rem"
                , Html.Attributes.type_ "number"
                , Html.Attributes.min "0"
                , Html.Attributes.step "1"
                , model.redAngle
                    |> Angle.inDegrees
                    |> String.fromFloat
                    |> Html.Attributes.value
                , Html.Events.onInput
                    (String.toFloat
                        >> Maybe.map Angle.degrees
                        >> Maybe.withDefault model.redAngle
                        >> UserEnteredRedAngle
                    )
                ]
                []
            , Html.input
                [ Html.Attributes.placeholder "Force (Newtons)"
                , Html.Attributes.style "font-size" "1.25rem"
                , Html.Attributes.type_ "number"
                , Html.Attributes.min "0"
                , Html.Attributes.step "1"
                , model.redForce
                    |> Force.inNewtons
                    |> String.fromFloat
                    |> Html.Attributes.value
                , Html.Events.onInput
                    (String.toFloat
                        >> Maybe.map Force.newtons
                        >> Maybe.withDefault model.redForce
                        >> UserEnteredRedForce
                    )
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "submit"
                ]
                [ Html.text "Fire!" ]
            ]
        ]
    }


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
                    (Point3d.meters -15 -15 0)
                    (Point3d.meters -15 15 0)
                    (Point3d.meters 15 15 0)
                    (Point3d.meters 15 -15 0)

            Block b c ->
                Scene3d.blockWithShadow
                    (Material.nonmetal
                        { baseColor = c
                        , roughness = 0.25
                        }
                    )
                    b

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
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrame (\_ -> Tick)
        ]


decodeMouseRay :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decoder msg
decodeMouseRay ( width, height ) rayToMsg =
    Json.Decode.map2
        (\x y ->
            rayToMsg <|
                Camera3d.ray camera
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
