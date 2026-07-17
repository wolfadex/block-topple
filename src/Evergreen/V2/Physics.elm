module Evergreen.V2.Physics exposing (..)

import Evergreen.V2.Internal.Coordinates
import Evergreen.V2.Physics.Types


type alias BodyCoordinates =
    Evergreen.V2.Internal.Coordinates.BodyCoordinates


type alias Body =
    Evergreen.V2.Physics.Types.Body


type alias Contacts id =
    Evergreen.V2.Physics.Types.Contacts id
