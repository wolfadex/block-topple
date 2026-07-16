module Css exposing (setupText, home, joinForm, waiting, gameCameraControls, loadingContainer, cube, s1, s2, s3, s4, s5, s6)

import Html
import Html.Attributes


setupText : Html.Attribute msg
setupText =
    Html.Attributes.class "setupText"


home : Html.Attribute msg
home =
    Html.Attributes.class "home"


joinForm : Html.Attribute msg
joinForm =
    Html.Attributes.class "joinForm"


waiting : Html.Attribute msg
waiting =
    Html.Attributes.class "waiting"


gameCameraControls : Html.Attribute msg
gameCameraControls =
    Html.Attributes.class "gameCameraControls"


loadingContainer : Html.Attribute msg
loadingContainer =
    Html.Attributes.class "loadingContainer"


cube : Html.Attribute msg
cube =
    Html.Attributes.class "cube"


s1 : Html.Attribute msg
s1 =
    Html.Attributes.class "s1"


s2 : Html.Attribute msg
s2 =
    Html.Attributes.class "s2"


s3 : Html.Attribute msg
s3 =
    Html.Attributes.class "s3"


s4 : Html.Attribute msg
s4 =
    Html.Attributes.class "s4"


s5 : Html.Attribute msg
s5 =
    Html.Attributes.class "s5"


s6 : Html.Attribute msg
s6 =
    Html.Attributes.class "s6"
