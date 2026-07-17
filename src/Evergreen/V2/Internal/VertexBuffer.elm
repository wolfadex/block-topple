module Evergreen.V2.Internal.VertexBuffer exposing (..)

import Evergreen.V2.Internal.Vector3


type VertexBuffer
    = Node Int Evergreen.V2.Internal.Vector3.Vec3 VertexBuffer VertexBuffer
    | Empty () () () ()
