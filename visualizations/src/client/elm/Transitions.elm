module Transitions exposing (..)

import Element exposing (..)
import Html
import Html.Attributes exposing (class, style)


animateAll : Element.Attribute msg
animateAll =
    htmlAttribute <| style "transition" "all 500ms ease-out"


growAndShrink : Element.Attribute msg
growAndShrink =
    htmlAttribute <| style "transition" "all 500ms ease-out"


animateFromRight : Element.Attribute msg
animateFromRight =
    htmlAttribute <| class "moveIn"


animateFadeIn : Element.Attribute msg
animateFadeIn =
    htmlAttribute <| class "fadeIn"
