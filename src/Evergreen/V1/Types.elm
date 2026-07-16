module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Color
import Duration
import Evergreen.V1.Block3d
import Evergreen.V1.Cone3d
import Evergreen.V1.Cylinder3d
import Evergreen.V1.Physics
import Evergreen.V1.Scene3d.Material
import Evergreen.V1.Scene3d.Mesh
import Evergreen.V1.Sphere3d
import Http
import Lamdera
import Length
import Pixels
import Quantity
import Random
import SeqDict
import SeqSet
import Timestep
import Url
import WebGL.Texture


type Turn
    = Red
    | Blue


type BlockType
    = Box (Evergreen.V1.Block3d.Block3d Length.Meters Evergreen.V1.Physics.BodyCoordinates)
    | Cylinder (Evergreen.V1.Cylinder3d.Cylinder3d Length.Meters Evergreen.V1.Physics.BodyCoordinates)
    | Cone (Evergreen.V1.Cone3d.Cone3d Length.Meters Evergreen.V1.Physics.BodyCoordinates)


type Id
    = Floor
    | Block BlockType Color.Color
    | RedBall (Evergreen.V1.Sphere3d.Sphere3d Length.Meters Evergreen.V1.Physics.BodyCoordinates) Color.Color
    | BlueBall (Evergreen.V1.Sphere3d.Sphere3d Length.Meters Evergreen.V1.Physics.BodyCoordinates) Color.Color


type Stage
    = Aiming
    | Simulating
    | GameOver Turn


type alias GameFrontend =
    { myColor : Turn
    , opponentDisconnected : Maybe Duration.Duration
    , bodies : List ( Id, Evergreen.V1.Physics.Body )
    , prevBodies : List ( Id, Evergreen.V1.Physics.Body )
    , contacts : Evergreen.V1.Physics.Contacts Id
    , elevantionRaw : String
    , rotationRaw : String
    , forceRaw : String
    , turn : Turn
    , stage : Stage
    , elapsed : Duration.Duration
    , timestep : Timestep.Timestep
    , cameraRotation : Float
    , redTowersRemaining : Int
    , blueTowersRemaining : Int
    }


type Page
    = Home String Bool
    | Waiting String
    | InGame GameFrontend
    | AdminView


type alias CustomMesh =
    ( Evergreen.V1.Scene3d.Mesh.Textured Evergreen.V1.Physics.BodyCoordinates, Evergreen.V1.Scene3d.Mesh.Shadow Evergreen.V1.Physics.BodyCoordinates )


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    , dimensions : ( Quantity.Quantity Int Pixels.Pixels, Quantity.Quantity Int Pixels.Pixels )
    , boxMesh : Maybe CustomMesh
    , boxMaterialRed : Maybe (Evergreen.V1.Scene3d.Material.Textured Evergreen.V1.Physics.BodyCoordinates)
    , boxMaterialBlue : Maybe (Evergreen.V1.Scene3d.Material.Textured Evergreen.V1.Physics.BodyCoordinates)
    , cylinderMesh : Maybe CustomMesh
    }


type alias Game =
    { players : ( ( Lamdera.SessionId, Turn ), ( Lamdera.SessionId, Turn ) )
    , bodies : List ( Id, Evergreen.V1.Physics.Body )
    , contacts : Evergreen.V1.Physics.Contacts Id
    , turn : Turn
    , stage : Stage
    , elapsed : Duration.Duration
    , timestep : Timestep.Timestep
    , redTowersRemaining : Int
    , blueTowersRemaining : Int
    }


type alias BackendModel =
    { waiting : Maybe Lamdera.SessionId
    , rooms : List ( Lamdera.SessionId, Lamdera.SessionId, Game )
    , hasLeft : SeqSet.SeqSet Lamdera.SessionId
    , seed : Random.Seed
    , waitingForFriend : SeqDict.SeqDict String Lamdera.SessionId
    , adminClient : Maybe Lamdera.ClientId
    }


type GameMsg
    = Tick Duration.Duration
    | UserEnteredElevation String
    | UserEnteredRotation String
    | UserEnteredForce String
    | UserFiredBall
    | UserStartedMovingCamera String
    | UserRequestedNewGame
    | UserRequestedLeaveMatch


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Resize Int Int
    | BoxMeshLoaded (Result Http.Error (Evergreen.V1.Scene3d.Mesh.Textured Evergreen.V1.Physics.BodyCoordinates))
    | BoxRedTextureLoaded (Result WebGL.Texture.Error (Evergreen.V1.Scene3d.Material.Texture Color.Color))
    | BoxBlueTextureLoaded (Result WebGL.Texture.Error (Evergreen.V1.Scene3d.Material.Texture Color.Color))
    | CylinderMeshLoaded (Result Http.Error (Evergreen.V1.Scene3d.Mesh.Textured Evergreen.V1.Physics.BodyCoordinates))
    | UserChosePlayWithStranger
    | UserChoseHostFriend
    | UserChangedJoinCode String
    | UserChoseJoinFriend
    | UserAbandonedWaiting
    | Admin_ClickedClearAllMatches
    | GameMessage GameMsg


type ToBackend
    = Fire Float Float Float
    | PlayWithStranger
    | HostFriend
    | JoinFriend String
    | AbandonWaiting
    | LeaveMatchRequested
    | Admin_ClearAllMatches


type alias TurnChangeGame =
    { bodies : List ( Id, Evergreen.V1.Physics.Body )
    , contacts : Evergreen.V1.Physics.Contacts Id
    , turn : Turn
    , stage : Stage
    , redTowersRemaining : Int
    , blueTowersRemaining : Int
    }


type BackendMsg
    = OnConnect Lamdera.SessionId Lamdera.ClientId
    | OnDisconnect Lamdera.SessionId Lamdera.ClientId
    | UserHasLeft Lamdera.SessionId
    | GameUpdateElapsed Lamdera.SessionId Lamdera.SessionId TurnChangeGame
    | SeedInitialized Random.Seed


type alias GameRejoin =
    { yourColor : Turn
    , bodies : List ( Id, Evergreen.V1.Physics.Body )
    , contacts : Evergreen.V1.Physics.Contacts Id
    , turn : Turn
    , stage : Stage
    , elapsed : Duration.Duration
    , timestep : Timestep.Timestep
    , redTowersRemaining : Int
    , blueTowersRemaining : Int
    }


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
    | Admin_LoggedIn
    | Admin_ForcedReset
