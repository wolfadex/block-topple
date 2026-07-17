module Evergreen.V2.Internal.Equation exposing (..)

import Evergreen.V2.Internal.Vector3


type alias WarmStart =
    { lambda : Float
    , t1 : Evergreen.V2.Internal.Vector3.Vec3
    }
