module Evergreen.V1.Internal.Body exposing (..)

import Evergreen.V1.Internal.Coordinates
import Evergreen.V1.Internal.Material
import Evergreen.V1.Internal.Matrix3
import Evergreen.V1.Internal.Shape
import Evergreen.V1.Internal.Transform3d
import Evergreen.V1.Internal.Vector3


type alias Geometry =
    { volume : Float
    , shapesWithMaterials : List ( Evergreen.V1.Internal.Shape.Shape Evergreen.V1.Internal.Shape.CenterOfMassCoordinates, Evergreen.V1.Internal.Material.Material )
    , boundingSphereRadius : Float
    }


type alias Body =
    { id : Int
    , kindInt : Int
    , transform3d :
        Evergreen.V1.Internal.Transform3d.Transform3d
            Evergreen.V1.Internal.Coordinates.WorldCoordinates
            { defines : Evergreen.V1.Internal.Shape.CenterOfMassCoordinates
            }
    , centerOfMassTransform3d :
        Evergreen.V1.Internal.Transform3d.Transform3d
            Evergreen.V1.Internal.Coordinates.BodyCoordinates
            { defines : Evergreen.V1.Internal.Shape.CenterOfMassCoordinates
            }
    , velocity : Evergreen.V1.Internal.Vector3.Vec3
    , angularVelocity : Evergreen.V1.Internal.Vector3.Vec3
    , mass : Float
    , geometry : Geometry
    , worldShapesWithMaterials : List ( Evergreen.V1.Internal.Shape.Shape Evergreen.V1.Internal.Coordinates.WorldCoordinates, Evergreen.V1.Internal.Material.Material )
    , force : Evergreen.V1.Internal.Vector3.Vec3
    , torque : Evergreen.V1.Internal.Vector3.Vec3
    , linearDamping : Float
    , angularDamping : Float
    , invMass : Float
    , invInertia : Evergreen.V1.Internal.Vector3.Vec3
    , invInertiaWorld : Evergreen.V1.Internal.Matrix3.Mat3
    , linearLock : Evergreen.V1.Internal.Vector3.Vec3
    , angularLock : Evergreen.V1.Internal.Vector3.Vec3
    }
