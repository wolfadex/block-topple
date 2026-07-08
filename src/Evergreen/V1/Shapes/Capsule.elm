module Evergreen.V1.Shapes.Capsule exposing (..)

import Evergreen.V1.Internal.Matrix3
import Evergreen.V1.Internal.Vector3


type alias Capsule =
    { radius : Float
    , halfLength : Float
    , axis : Evergreen.V1.Internal.Vector3.Vec3
    , position : Evergreen.V1.Internal.Vector3.Vec3
    , volume : Float
    , inertia : Evergreen.V1.Internal.Matrix3.Mat3
    }
