module Evergreen.V1.Scene3d.Types exposing (..)

import Evergreen.V1.BoundingBox3d
import Evergreen.V1.LineSegment3d
import Evergreen.V1.Point3d
import Evergreen.V1.Polyline3d
import Evergreen.V1.Triangle3d
import Evergreen.V1.Vector3d
import Length
import Luminance
import Math.Vector2
import Math.Vector3
import Math.Vector4
import Quantity
import TriangularMesh
import WebGL
import WebGL.Texture


type alias PlainVertex =
    { position : Math.Vector3.Vec3
    }


type BackFaceSetting
    = KeepBackFaces
    | CullBackFaces


type alias VertexWithNormal =
    { position : Math.Vector3.Vec3
    , normal : Math.Vector3.Vec3
    }


type alias VertexWithUv =
    { position : Math.Vector3.Vec3
    , uv : Math.Vector2.Vec2
    }


type alias VertexWithNormalAndUv =
    { position : Math.Vector3.Vec3
    , normal : Math.Vector3.Vec3
    , uv : Math.Vector2.Vec2
    }


type alias VertexWithTangent =
    { position : Math.Vector3.Vec3
    , normal : Math.Vector3.Vec3
    , uv : Math.Vector2.Vec2
    , tangent : Math.Vector4.Vec4
    }


type Mesh coordinates attributes
    = EmptyMesh
    | Triangles (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates) (List (Evergreen.V1.Triangle3d.Triangle3d Length.Meters coordinates)) (WebGL.Mesh PlainVertex) BackFaceSetting
    | Facets (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates) (List (Evergreen.V1.Triangle3d.Triangle3d Length.Meters coordinates)) (WebGL.Mesh VertexWithNormal) BackFaceSetting
    | Indexed (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates) (TriangularMesh.TriangularMesh (Evergreen.V1.Point3d.Point3d Length.Meters coordinates)) (WebGL.Mesh PlainVertex) BackFaceSetting
    | MeshWithNormals
        (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates)
        (TriangularMesh.TriangularMesh
            { position : Evergreen.V1.Point3d.Point3d Length.Meters coordinates
            , normal : Evergreen.V1.Vector3d.Vector3d Quantity.Unitless coordinates
            }
        )
        (WebGL.Mesh VertexWithNormal)
        BackFaceSetting
    | MeshWithUvs
        (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates)
        (TriangularMesh.TriangularMesh
            { position : Evergreen.V1.Point3d.Point3d Length.Meters coordinates
            , uv : ( Float, Float )
            }
        )
        (WebGL.Mesh VertexWithUv)
        BackFaceSetting
    | MeshWithNormalsAndUvs
        (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates)
        (TriangularMesh.TriangularMesh
            { position : Evergreen.V1.Point3d.Point3d Length.Meters coordinates
            , normal : Evergreen.V1.Vector3d.Vector3d Quantity.Unitless coordinates
            , uv : ( Float, Float )
            }
        )
        (WebGL.Mesh VertexWithNormalAndUv)
        BackFaceSetting
    | MeshWithTangents
        (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates)
        (TriangularMesh.TriangularMesh
            { position : Evergreen.V1.Point3d.Point3d Length.Meters coordinates
            , normal : Evergreen.V1.Vector3d.Vector3d Quantity.Unitless coordinates
            , uv : ( Float, Float )
            , tangent : Evergreen.V1.Vector3d.Vector3d Quantity.Unitless coordinates
            , tangentBasisIsRightHanded : Bool
            }
        )
        (WebGL.Mesh VertexWithTangent)
        BackFaceSetting
    | LineSegments (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates) (List (Evergreen.V1.LineSegment3d.LineSegment3d Length.Meters coordinates)) (WebGL.Mesh PlainVertex)
    | Polyline (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates) (Evergreen.V1.Polyline3d.Polyline3d Length.Meters coordinates) (WebGL.Mesh PlainVertex)
    | Points (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates) Float (List (Evergreen.V1.Point3d.Point3d Length.Meters coordinates)) (WebGL.Mesh PlainVertex)


type Shadow coordinates
    = EmptyShadow
    | Shadow (Evergreen.V1.BoundingBox3d.BoundingBox3d Length.Meters coordinates) (TriangularMesh.TriangularMesh VertexWithNormal) (WebGL.Mesh VertexWithNormal)


type TextureMap
    = UseMeshUvs


type Texture value
    = Constant value
    | Texture
        { url : String
        , options : WebGL.Texture.Options
        , data : WebGL.Texture.Texture
        }


type LinearRgb units
    = LinearRgb Math.Vector4.Vec4


type NormalMapFormat
    = OpenglFormat
    | DirectxFormat


type NormalMap
    = NoNormalMap
    | NormalMap
        { url : String
        , options : WebGL.Texture.Options
        , data : WebGL.Texture.Texture
        , format : NormalMapFormat
        }


type Material coordinates attributes
    = UnlitMaterial TextureMap (Texture Math.Vector4.Vec4)
    | EmissiveMaterial TextureMap (Texture (LinearRgb Quantity.Unitless)) Luminance.Luminance
    | LambertianMaterial TextureMap (Texture (LinearRgb Quantity.Unitless)) (Texture Float) NormalMap
    | PbrMaterial TextureMap (Texture (LinearRgb Quantity.Unitless)) (Texture Float) (Texture Float) (Texture Float) NormalMap
