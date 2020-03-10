module ColorsDashboard exposing (asC, blue, darkGrey, green, grey, lightBlue, lightGrey, moduleGrey, orange, pink, purple, red, rgbac255, white)

import Color
import Color.Convert exposing (hexToColor)
import Element


rgbac255 : Int -> Int -> Int -> Float -> Color.Color
rgbac255 r g b a =
    Color.fromRgba
        { red = toFloat r / 255
        , green = toFloat g / 255
        , blue = toFloat b / 255
        , alpha = a
        }


asC : Element.Color -> Color.Color
asC ec =
    let
        ecc =
            Element.toRgb ec
    in
    Color.fromRgba ecc


fromHex : String -> Element.Color
fromHex str =
    case hexToColor str of
        Ok col ->
            let
                x =
                    Color.toRgba col
            in
            Element.rgba x.red x.green x.blue x.alpha

        Err _ ->
            Element.rgb 255 0 0


lightGrey =
    Element.rgb255 140 145 149


darkGrey =
    fromHex "#0A1117"


moduleGrey =
    fromHex "#121922"


purple =
    Element.rgb255 191 60 255


lightBlue =
    Element.rgb255 0 192 255


pink =
    Element.rgb255 255 43 188


green =
    Element.rgb255 0 255 139


grey =
    Element.rgb255 238 238 238


white =
    Element.rgb 1 1 1


red =
    Element.rgb255 255 54 54


blue =
    Element.rgb255 0 138 255


orange =
    Element.rgb255 232 138 80
