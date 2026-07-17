module Evergreen.V2.Scene3d.Material exposing (..)

import Evergreen.V2.Scene3d.Types


type alias Material coordinates attributes =
    Evergreen.V2.Scene3d.Types.Material coordinates attributes


type alias Textured coordinates =
    Material
        coordinates
        { normals : ()
        , uvs : ()
        }


type alias Texture value =
    Evergreen.V2.Scene3d.Types.Texture value
