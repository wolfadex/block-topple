module Evergreen.V1.Internal.ContactCache exposing (..)


type NColor
    = Red
    | Black


type ContactCache v
    = Node NColor Int (List ( Int, Int, v )) (ContactCache v) (ContactCache v)
    | Empty () () () () ()
