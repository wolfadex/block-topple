module Evergreen.V2.Shapes.Convex exposing (..)

import Evergreen.V2.Internal.Matrix3
import Evergreen.V2.Internal.Vector3
import Evergreen.V2.Internal.VertexBuffer


type FaceGroup
    = OneSidedFace Evergreen.V2.Internal.Vector3.Vec3 (List Int) Float () () ()
    | TwoSidedFace Evergreen.V2.Internal.Vector3.Vec3 (List Int) Float Evergreen.V2.Internal.Vector3.Vec3 (List Int) Float


type Obb
    = Box Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3 Evergreen.V2.Internal.Vector3.Vec3
    | NotBox (List Evergreen.V2.Internal.Vector3.Vec3) () () ()


type alias Convex =
    { faces : List FaceGroup
    , uniqueEdges : List (List Int)
    , vertexBuffer : Evergreen.V2.Internal.VertexBuffer.VertexBuffer
    , obb : Obb
    , position : Evergreen.V2.Internal.Vector3.Vec3
    , inertia : Evergreen.V2.Internal.Matrix3.Mat3
    , volume : Float
    }
