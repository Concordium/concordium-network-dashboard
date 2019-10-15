module Transitions exposing (..)

import Element exposing (..)
import Html
import Html.Attributes exposing (class, style)


growAndShrink : Element.Attribute msg
growAndShrink =
    htmlAttribute <| style "transition" "all 500ms ease-out"


animateFromRight : Element.Attribute msg
animateFromRight =
    htmlAttribute <| class "scaleIn"


animateFadeIn : Element.Attribute msg
animateFadeIn =
    htmlAttribute <| class "fadeIn"
