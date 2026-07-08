module Evergreen.V1.Internal.Equation exposing (..)

import Evergreen.V1.Internal.Vector3


type alias WarmStart =
    { lambda : Float
    , t1 : Evergreen.V1.Internal.Vector3.Vec3
    }
