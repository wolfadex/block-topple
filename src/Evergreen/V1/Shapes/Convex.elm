module Evergreen.V1.Shapes.Convex exposing (..)

import Evergreen.V1.Internal.Matrix3
import Evergreen.V1.Internal.Vector3
import Evergreen.V1.Internal.VertexBuffer


type FaceGroup
    = OneSidedFace Evergreen.V1.Internal.Vector3.Vec3 (List Int) Float () () ()
    | TwoSidedFace Evergreen.V1.Internal.Vector3.Vec3 (List Int) Float Evergreen.V1.Internal.Vector3.Vec3 (List Int) Float


type Obb
    = Box Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3
    | NotBox (List Evergreen.V1.Internal.Vector3.Vec3) () () ()


type alias Convex =
    { faces : List FaceGroup
    , uniqueEdges : List (List Int)
    , vertexBuffer : Evergreen.V1.Internal.VertexBuffer.VertexBuffer
    , obb : Obb
    , position : Evergreen.V1.Internal.Vector3.Vec3
    , inertia : Evergreen.V1.Internal.Matrix3.Mat3
    , volume : Float
    }
