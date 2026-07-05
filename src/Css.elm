module Css exposing (home, joinForm, waiting)

import Html
import Html.Attributes


home : Html.Attribute msg
home =
    Html.Attributes.class "home"


joinForm : Html.Attribute msg
joinForm =
    Html.Attributes.class "joinForm"


waiting : Html.Attribute msg
waiting =
    Html.Attributes.class "waiting"
