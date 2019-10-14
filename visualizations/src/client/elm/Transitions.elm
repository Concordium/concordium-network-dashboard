module Transitions exposing (..)

import Element exposing (..)
import Html
import Html.Attributes exposing (style)


growAndShrink : Element.Attribute msg
growAndShrink =
    htmlAttribute <| style "transition" "all 500ms ease-out"
