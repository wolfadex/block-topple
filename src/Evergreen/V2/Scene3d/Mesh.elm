module Evergreen.V2.Scene3d.Mesh exposing (..)

import Evergreen.V2.Scene3d.Types


type alias Mesh coordinates attributes =
    Evergreen.V2.Scene3d.Types.Mesh coordinates attributes


type alias Textured coordinates =
    Mesh
        coordinates
        { normals : ()
        , uvs : ()
        }


type alias Shadow coordinates =
    Evergreen.V2.Scene3d.Types.Shadow coordinates
