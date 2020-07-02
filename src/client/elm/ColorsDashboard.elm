module ColorsDashboard exposing (asC, blue,, green, grey, orange, pink, purple, red, white)

import Color
import Element


asC : Element.Color -> Color.Color
asC ec =
    let
        ecc =
            Element.toRgb ec
    in
    Color.fromRgba ecc


purple =
    Element.rgb255 191 60 255


pink =
    Element.rgb255 255 43 188


green =
    Element.rgb255 0 255 139


grey =
    Element.rgb255 238 238 238


white =
    Element.rgb255 255 255 255


red =
    Element.rgb255 255 54 54


blue =
    Element.rgb255 0 138 255


orange =
    Element.rgb255 232 138 80
