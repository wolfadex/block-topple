module Evergreen.V1.Physics exposing (..)

import Evergreen.V1.Internal.Coordinates
import Evergreen.V1.Physics.Types


type alias BodyCoordinates =
    Evergreen.V1.Internal.Coordinates.BodyCoordinates


type alias Body =
    Evergreen.V1.Physics.Types.Body


type alias Contacts id =
    Evergreen.V1.Physics.Types.Contacts id
