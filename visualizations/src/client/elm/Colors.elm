module Colors exposing
    ( background
    , blue
    , blueishBlack
    , blueishVeryDarkGray
    , fadeToBackground
    , fromUI
    , green
    , lightGray
    , midGray
    , nodeBackground
    , pink
    , purple
    , toUI
    , white
    , yellow
    )

import Color exposing (Color, rgb255)
import Color.Convert exposing (hexToColor)
import Color.Interpolate exposing (Space(..), interpolate)
import Element


background =
    blueishBlack


nodeBackground =
    blueishVeryDarkGray


blueishBlack =
    fromHex "#0a1117"


blueishVeryDarkGray =
    fromHex "#121922"


green =
    fromHex "#80ffb3"


pink =
    fromHex "#ff9fff"


blue =
    fromHex "#2190f8"


lightBlue =
    fromHex "#66dbff"


purple =
    fromHex "#bf88ff"


yellow =
    fromHex "#ffde80"


midGray =
    fromHex "#666677"


lightGray =
    fromHex "#9999aa"


white =
    fromHex "#fffff"


fromHex : String -> Color
fromHex hex =
    hexToColor hex |> Result.withDefault (rgb255 0 0 0)


toUI : Color -> Element.Color
toUI color =
    let
        c =
            Color.toRgba color
    in
    Element.rgb c.red c.green c.blue


fromUI : Element.Color -> Color
fromUI color =
    let
        c =
            Element.toRgb color
    in
    Color.rgb c.red c.green c.blue


fadeToBackground : Float -> Color -> Color
fadeToBackground alpha color =
    interpolate LAB color background alpha
