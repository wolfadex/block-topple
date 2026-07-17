module Evergreen.V2.Internal.Contact exposing (..)

import Evergreen.V2.Internal.Body
import Evergreen.V2.Internal.Constraint
import Evergreen.V2.Internal.Shape
import Evergreen.V2.Internal.Vector3


type alias Contact =
    { shapeKey : Int
    , featureKey : Int
    , ni : Evergreen.V2.Internal.Vector3.Vec3
    , pi : Evergreen.V2.Internal.Vector3.Vec3
    , pj : Evergreen.V2.Internal.Vector3.Vec3
    }


type alias SolverContact =
    { friction : Float
    , bounciness : Float
    , contact : Contact
    }


type alias PairGroup =
    { body1 : Evergreen.V2.Internal.Body.Body
    , body2 : Evergreen.V2.Internal.Body.Body
    , contacts : List SolverContact
    , constraints : List (Evergreen.V2.Internal.Constraint.Constraint Evergreen.V2.Internal.Shape.CenterOfMassCoordinates)
    }
