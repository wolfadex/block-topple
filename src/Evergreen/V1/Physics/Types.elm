module Evergreen.V1.Physics.Types exposing (..)

import Array
import Evergreen.V1.Internal.Body
import Evergreen.V1.Internal.Contact
import Evergreen.V1.Internal.ContactCache
import Evergreen.V1.Internal.Equation


type Body
    = Body Evergreen.V1.Internal.Body.Body


type Contacts id
    = Contacts
        { warmStart : Evergreen.V1.Internal.ContactCache.ContactCache Evergreen.V1.Internal.Equation.WarmStart
        , iterations : Int
        , pairGroups : List Evergreen.V1.Internal.Contact.PairGroup
        , bodies : Array.Array ( id, Evergreen.V1.Internal.Body.Body )
        }
