module Evergreen.V2.Geometry.Types exposing (..)

import Quantity


type Point3d units coordinates
    = Point3d
        { x : Float
        , y : Float
        , z : Float
        }


type Direction3d coordinates
    = Direction3d
        { x : Float
        , y : Float
        , z : Float
        }


type Frame3d units coordinates defines
    = Frame3d
        { originPoint : Point3d units coordinates
        , xDirection : Direction3d coordinates
        , yDirection : Direction3d coordinates
        , zDirection : Direction3d coordinates
        }


type Block3d units coordinates
    = Block3d
        { axes :
            Frame3d
                units
                coordinates
                {}
        , dimensions : ( Quantity.Quantity Float units, Quantity.Quantity Float units, Quantity.Quantity Float units )
        }


type Axis3d units coordinates
    = Axis3d
        { originPoint : Point3d units coordinates
        , direction : Direction3d coordinates
        }


type Cylinder3d units coordinates
    = Cylinder3d
        { axis : Axis3d units coordinates
        , radius : Quantity.Quantity Float units
        , length : Quantity.Quantity Float units
        }


type Cone3d units coordinates
    = Cone3d
        { axis : Axis3d units coordinates
        , radius : Quantity.Quantity Float units
        , length : Quantity.Quantity Float units
        }


type Sphere3d units coordinates
    = Sphere3d
        { centerPoint : Point3d units coordinates
        , radius : Quantity.Quantity Float units
        }


type BoundingBox3d units coordinates
    = BoundingBox3d
        { minX : Float
        , maxX : Float
        , minY : Float
        , maxY : Float
        , minZ : Float
        , maxZ : Float
        }


type Triangle3d units coordinates
    = Triangle3d ( Point3d units coordinates, Point3d units coordinates, Point3d units coordinates )


type Vector3d units coordinates
    = Vector3d
        { x : Float
        , y : Float
        , z : Float
        }


type LineSegment3d units coordinates
    = LineSegment3d ( Point3d units coordinates, Point3d units coordinates )


type Polyline3d units coordinates
    = Polyline3d (List (Point3d units coordinates))
