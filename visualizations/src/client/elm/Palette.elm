module Palette exposing
    ( ColorMode(..)
    , Palette
    , colorToUI
    , defaultDark
    , defaultLight
    , fadeWithoutAlpha
    , mapPalette
    , toHex
    , uiToColor
    , withAlphaCo
    , withAlphaEl
    )

import Color exposing (Color, rgb255)
import Color.Convert exposing (colorToHex, hexToColor)
import Color.Interpolate exposing (Space(..), interpolate)
import Element


type ColorMode
    = Light
    | Dark


type alias Palette a =
    { bg1 : a
    , bg2 : a
    , bg3 : a
    , fg1 : a
    , fg2 : a
    , fg3 : a
    , c1 : a
    , c2 : a
    , c3 : a
    , c4 : a
    , c5 : a
    , success : a
    , failure : a
    , danger : a
    , warning : a
    , info : a
    , deactivated : a
    }


defaultDark : Palette Element.Color
defaultDark =
    { bg1 = blueishBlack
    , bg2 = blueishVeryDarkGray
    , bg3 = blueishDarkGray
    , fg1 = nearWhite
    , fg2 = blueishLightGray
    , fg3 = blueishGray
    , c1 = blue
    , c2 = green
    , c3 = purple
    , c4 = pink
    , c5 = yellow
    , success = green
    , failure = red
    , danger = orange
    , warning = yellow
    , info = midGray
    , deactivated = blueishGray
    }


defaultLight : Palette Element.Color
defaultLight =
    { bg1 = fromHex "#f8f0e0"
    , bg2 = white
    , bg3 = lightBrown
    , fg1 = fromHex "#181817"
    , fg2 = darkBrown
    , fg3 = blueishVeryDarkGray
    , c1 = lightBlueNew
    , c2 = brown
    , c3 = purple
    , c4 = pink
    , c5 = yellow
    , success = midGray
    , failure = red
    , danger = orange
    , warning = yellow
    , info = fromHex "#A0A0A0"
    , deactivated = blueishGray
    }


mapPalette : (a -> b) -> Palette a -> Palette b
mapPalette fn original =
    { bg1 = fn original.bg1
    , bg2 = fn original.bg2
    , bg3 = fn original.bg3
    , fg1 = fn original.fg1
    , fg2 = fn original.fg2
    , fg3 = fn original.fg3
    , c1 = fn original.c1
    , c2 = fn original.c2
    , c3 = fn original.c3
    , c4 = fn original.c4
    , c5 = fn original.c5
    , warning = fn original.warning
    , failure = fn original.failure
    , success = fn original.success
    , danger = fn original.danger
    , info = fn original.info
    , deactivated = fn original.deactivated
    }



-- Light


brownishLightGray =
    fromHex ""


lightBrown =
    fromHex "#C0B2A4"


brown =
    fromHex "#8F8479"


darkBrown =
    fromHex "#343231"


lightBlueNew =
    fromHex "#4486AB"


mediumBlue =
    fromHex "#325D76"



-- DARK


blueishBlack =
    fromHex "#0a1117"


blueishVeryDarkGray =
    fromHex "#121922"


green =
    fromHex "#80ffb3"


pink =
    fromHex "#fa9fff"


blue =
    fromHex "#2190f8"


lightBlue =
    fromHex "#66dbff"


purple =
    fromHex "#bf88ff"


yellow =
    fromHex "#ffde80"


red =
    fromHex "#c64b55"


orange =
    fromHex "#e0926b"


blueishLightGray =
    fromHex "#687181"


blueishDarkGray =
    fromHex "#232933"


blueishGray =
    fromHex "#3D4657"


midGray =
    fromHex "#666677"


lightGray =
    fromHex "#9999aa"


nearWhite =
    fromHex "#eeeeee"


veryLightGray =
    fromHex "#f7f7f4"


white =
    fromHex "#ffffff"



-- HELPER FUNCTIONS
-- Conversion


fromHex : String -> Element.Color
fromHex hex =
    hexToColor hex
        |> Result.withDefault (rgb255 0 1 0)
        |> colorToUI


toHex : Element.Color -> String
toHex color =
    uiToColor color
        |> colorToHex


colorToUI : Color -> Element.Color
colorToUI =
    Element.fromRgb << Color.toRgba


uiToColor : Element.Color -> Color
uiToColor =
    Color.fromRgba << Element.toRgb



-- Modification


withAlphaCo : Float -> Color -> Color
withAlphaCo alpha color =
    let
        colorRgba =
            Color.toRgba color
    in
    Color.fromRgba { colorRgba | alpha = alpha }


withAlphaEl : Float -> Element.Color -> Element.Color
withAlphaEl alpha color =
    let
        crgb =
            Element.toRgb color
    in
    Element.fromRgb { crgb | alpha = alpha }


fadeWithoutAlpha : Float -> Color -> Color -> Color
fadeWithoutAlpha t colorA colorB =
    let
        colorAlpha =
            (Color.toRgba colorA).alpha
    in
    interpolate LAB colorA colorB t
        |> withAlphaCo colorAlpha
