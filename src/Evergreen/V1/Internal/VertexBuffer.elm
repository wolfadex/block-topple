module Evergreen.V1.Internal.VertexBuffer exposing (..)

import Evergreen.V1.Internal.Vector3


type VertexBuffer
    = Node Int Evergreen.V1.Internal.Vector3.Vec3 VertexBuffer VertexBuffer
    | Empty () () () ()
