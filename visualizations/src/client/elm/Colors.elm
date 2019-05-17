module Colors exposing
    ( background
    , blue
    , green
    , nodeBackground
    , pink
    , purple
    , yellow
    )

import Color exposing (Color, rgb255)
import Color.Convert exposing (hexToColor)
import Color.Interpolate exposing (interpolate)


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


fromHex : String -> Color
fromHex hex =
    hexToColor hex |> Result.withDefault (rgb255 0 0 0)
