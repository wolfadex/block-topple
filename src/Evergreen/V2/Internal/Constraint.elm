module Evergreen.V2.Internal.Constraint exposing (..)

import Evergreen.V2.Internal.Vector3


type Constraint coordinates
    = PointToPoint Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3
    | Hinge Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3
    | Lock Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3
    | Distance Float
