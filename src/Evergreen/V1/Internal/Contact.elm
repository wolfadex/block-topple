module Evergreen.V1.Internal.Contact exposing (..)

import Evergreen.V1.Internal.Body
import Evergreen.V1.Internal.Constraint
import Evergreen.V1.Internal.Shape
import Evergreen.V1.Internal.Vector3


type alias Contact =
    { shapeKey : Int
    , featureKey : Int
    , ni : Evergreen.V1.Internal.Vector3.Vec3
    , pi : Evergreen.V1.Internal.Vector3.Vec3
    , pj : Evergreen.V1.Internal.Vector3.Vec3
    }


type alias SolverContact =
    { friction : Float
    , bounciness : Float
    , contact : Contact
    }


type alias PairGroup =
    { body1 : Evergreen.V1.Internal.Body.Body
    , body2 : Evergreen.V1.Internal.Body.Body
    , contacts : List SolverContact
    , constraints : List (Evergreen.V1.Internal.Constraint.Constraint Evergreen.V1.Internal.Shape.CenterOfMassCoordinates)
    }
