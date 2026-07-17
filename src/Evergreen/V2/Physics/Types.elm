module Evergreen.V2.Physics.Types exposing (..)

import Array
import Evergreen.V2.Internal.Body
import Evergreen.V2.Internal.Contact
import Evergreen.V2.Internal.ContactCache
import Evergreen.V2.Internal.Equation


type Body
    = Body Evergreen.V2.Internal.Body.Body


type Contacts id
    = Contacts
        { warmStart : Evergreen.V2.Internal.ContactCache.ContactCache Evergreen.V2.Internal.Equation.WarmStart
        , iterations : Int
        , pairGroups : List Evergreen.V2.Internal.Contact.PairGroup
        , bodies : Array.Array ( id, Evergreen.V2.Internal.Body.Body )
        }
