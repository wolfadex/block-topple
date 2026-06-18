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
import Url exposing (Url)
import Vector3d exposing (Vector3d)
import WebGL.Texture


type alias FrontendModel =
    { key : Key
    , bodies : List ( Id, Body )
    , prevBodies : List ( Id, Body )
    , contacts : Physics.Contacts Id
    , dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
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


type alias BackendModel =
    { message : String
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



--


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Resize Int Int
    | BoxMeshLoaded (Result Http.Error (Scene3d.Mesh.Textured BodyCoordinates))
    | BoxRedTextureLoaded (Result WebGL.Texture.Error (Scene3d.Material.Texture Color))
    | BoxBlueTextureLoaded (Result WebGL.Texture.Error (Scene3d.Material.Texture Color))
    | CylinderMeshLoaded (Result Http.Error (Scene3d.Mesh.Textured BodyCoordinates))
    | Tick Float
    | UserEnteredElevation String
    | UserEnteredRotation String
    | UserEnteredForce String
    | UserFiredBall
    | UserRotatedCamera String


type ToBackend
    = NoOpToBackend



--


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
