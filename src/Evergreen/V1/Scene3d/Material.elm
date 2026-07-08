module Evergreen.V1.Scene3d.Material exposing (..)

import Evergreen.V1.Scene3d.Types


type alias Material coordinates attributes =
    Evergreen.V1.Scene3d.Types.Material coordinates attributes


type alias Textured coordinates =
    Material
        coordinates
        { normals : ()
        , uvs : ()
        }


type alias Texture value =
    Evergreen.V1.Scene3d.Types.Texture value
