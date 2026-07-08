module Types exposing (..)

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
import Lamdera exposing (ClientId, SessionId)
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
import Random
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material
import Scene3d.Mesh
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Sphere3d exposing (Sphere3d)
import Task
import Timestep exposing (Timestep)
import TriangularMesh exposing (TriangularMesh)
import Url exposing (Url)
import Vector3d exposing (Vector3d)
import WebGL.Texture


type alias FrontendModel =
    { key : Key
    , page : Page
    , dimensions : ( Quantity Int Pixels, Quantity Int Pixels )

    --
    , boxMesh : Maybe CustomMesh
    , boxMaterialRed : Maybe (Scene3d.Material.Textured BodyCoordinates)
    , boxMaterialBlue : Maybe (Scene3d.Material.Textured BodyCoordinates)
    , cylinderMesh : Maybe CustomMesh
    }


type Page
    = Home String Bool
    | Waiting String
    | InGame GameFrontend
      --
    | AdminView


type alias GameFrontend =
    { myColor : Turn
    , opponentDisconnected : Maybe Duration
    , bodies : List ( Id, Body )
    , prevBodies : List ( Id, Body )
    , contacts : Physics.Contacts Id
    , elevantionRaw : String
    , rotationRaw : String
    , forceRaw : String
    , turn : Turn
    , stage : Stage
    , elapsed : Duration
    , timestep : Timestep
    , cameraRotation : Float
    , redTowersRemaining : Int
    , blueTowersRemaining : Int
    }


type alias BackendModel =
    { waiting : Maybe SessionId
    , rooms : List ( SessionId, SessionId, Game )
    , hasLeft : SeqSet SessionId
    , seed : Random.Seed
    , waitingForFriend : SeqDict String SessionId
    , adminClient : Maybe Lamdera.ClientId
    }


type alias Game =
    { players : ( ( SessionId, Turn ), ( SessionId, Turn ) )
    , bodies : List ( Id, Body )
    , contacts : Physics.Contacts Id
    , turn : Turn
    , stage : Stage
    , elapsed : Duration
    , timestep : Timestep
    , redTowersRemaining : Int
    , blueTowersRemaining : Int
    }


type Id
    = Floor
    | Block BlockType Color
    | RedBall (Sphere3d Meters BodyCoordinates) Color
    | BlueBall (Sphere3d Meters BodyCoordinates) Color


type BlockType
    = Box (Block3d Meters BodyCoordinates)
    | Cylinder (Cylinder3d Meters BodyCoordinates)
    | Cone (Cone3d Meters BodyCoordinates)


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
    | GameOver Turn



--


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Resize Int Int
    | BoxMeshLoaded (Result Http.Error (Scene3d.Mesh.Textured BodyCoordinates))
    | BoxRedTextureLoaded (Result WebGL.Texture.Error (Scene3d.Material.Texture Color))
    | BoxBlueTextureLoaded (Result WebGL.Texture.Error (Scene3d.Material.Texture Color))
    | CylinderMeshLoaded (Result Http.Error (Scene3d.Mesh.Textured BodyCoordinates))
      --
    | UserChosePlayWithStranger
    | UserChoseHostFriend
    | UserChangedJoinCode String
    | UserChoseJoinFriend
    | UserAbandonedWaiting
      --
    | Admin_ClickedClearAllMatches
      --
    | GameMessage GameMsg


type GameMsg
    = Tick Duration
    | UserEnteredElevation String
    | UserEnteredRotation String
    | UserEnteredForce String
    | UserFiredBall
    | UserRotatedCamera String
    | UserRequestedNewGame
    | UserRequestedLeaveMatch


type ToBackend
    = Fire Float Float Float
    | PlayWithStranger
    | HostFriend
    | JoinFriend String
    | AbandonWaiting
    | LeaveMatchRequested
      --
    | Admin_ClearAllMatches



--


type BackendMsg
    = OnConnect SessionId ClientId
    | OnDisconnect SessionId ClientId
    | UserHasLeft SessionId
    | GameUpdateElapsed SessionId SessionId TurnChangeGame
    | SeedInitialized Random.Seed


type ToFrontend
    = GameStarted Turn
    | OtherPlayerLeft
    | GameRejoined GameRejoin
    | TurnChange TurnChangeGame
    | OtherPlayerFired Float Float Float
    | OpponentDisconnected
    | OpponentConnected
    | OpponentLeft
    | BeginWaitingForStranger
    | BeginWaitingForFriend String
    | UnknownJoinCode
      --
    | Admin_LoggedIn
    | Admin_ForcedReset


type alias GameRejoin =
    { yourColor : Turn
    , bodies : List ( Id, Body )
    , contacts : Physics.Contacts Id
    , turn : Turn
    , stage : Stage
    , elapsed : Duration
    , timestep : Timestep
    , redTowersRemaining : Int
    , blueTowersRemaining : Int
    }


type alias TurnChangeGame =
    { bodies : List ( Id, Body )
    , contacts : Physics.Contacts Id
    , turn : Turn
    , stage : Stage
    , redTowersRemaining : Int
    , blueTowersRemaining : Int
    }



--
--
--


initBodiesTest : List ( Id, Body )
initBodiesTest =
    List.concat
        [ [ ( Floor, Physics.plane Plane3d.xy Physics.Material.wood )
          , initRedBall
          ]
        , initTower -800 0 Color.lightBlue
        , initTower 800 0 Color.lightRed
        ]


initBodies : List ( Id, Body )
initBodies =
    initBodiesBoxCastle


initBodiesBoxCastle : List ( Id, Body )
initBodiesBoxCastle =
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
      ( Block (Cone cone) color
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



--


countAllTowers : List ( Id, Body ) -> ( Int, Int )
countAllTowers bodies =
    List.foldl
        (\( id, _ ) (( blue, red ) as total) ->
            case id of
                Block (Cone _) color ->
                    if color == Color.lightRed then
                        ( blue, red + 1 )

                    else
                        ( blue + 1, red )

                _ ->
                    total
        )
        ( 0, 0 )
        bodies


countFallenTowers : Physics.Contacts Id -> ( Int, Int )
countFallenTowers contacts =
    contacts
        |> Physics.contactPoints
            (\left right ->
                case left of
                    Block (Cone _) _ ->
                        case right of
                            Floor ->
                                True

                            _ ->
                                False

                    Floor ->
                        case right of
                            Block (Cone _) _ ->
                                True

                            _ ->
                                False

                    _ ->
                        False
            )
        |> List.foldl
            (\( left, right, _ ) (( blue, red ) as total) ->
                case left of
                    Block (Cone _) color ->
                        if color == Color.lightRed then
                            ( blue, red + 1 )

                        else
                            ( blue + 1, red )

                    Floor ->
                        case right of
                            Block (Cone _) color ->
                                if color == Color.lightRed then
                                    ( blue, red + 1 )

                                else
                                    ( blue + 1, red )

                            _ ->
                                total

                    _ ->
                        total
            )
            ( 0, 0 )



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
