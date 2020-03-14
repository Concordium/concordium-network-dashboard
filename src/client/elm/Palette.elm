module Palette exposing
    ( Palette
    , blue
    , blueishDarkGray
    , blueishGray
    , colorToUI
    , dark
    , darkish
    , defaultDark
    , defaultLight
    , desaturate
    , green
    , invert
    , light
    , lightGray
    , lightish
    , mapPalette
    , midGray
    , nearWhite
    , pink
    , purple
    , toHex
    , uiToHsluv
    , veryDark
    , veryLight
    , white
    , withAlpha
    , yellow
    )

import Color exposing (Color, rgb255)
import Color.Convert exposing (colorToHex, hexToColor)
import Color.Interpolate exposing (interpolate)
import Element
import HSLuv exposing (..)
import HSLuv.Manipulate exposing (..)

type ColorMode = LightMode | DarkMode

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
    , deactivated = dark blueishGray
    }


defaultLight : Palette Element.Color
defaultLight =
    mapPalette invert defaultDark


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


blueishBlack =
    fromHex "#0a1117"


blueishVeryDarkGray =
    fromHex "#121922"


green =
    fromHex "#80ffb3"


pink =
    fromHex "#fa9fff"


pink2 =
    Element.rgb255 255 43 188


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


white =
    fromHex "#ffffff"


nearWhite =
    fromHex "#eeeeee"



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
colorToUI color =
    Element.fromRgb (Color.toRgba color)


uiToColor : Element.Color -> Color
uiToColor color =
    Color.fromRgba (Element.toRgb color)


hsluvToUi =
    Element.fromRgb << HSLuv.toRgba


uiToHsluv =
    HSLuv.rgba << Element.toRgb



-- Modification


withAlpha : Float -> Element.Color -> Element.Color
withAlpha alpha color =
    let
        crgb =
            Element.toRgb color
    in
    Element.fromRgb { crgb | alpha = alpha }


apply : (HSLuv -> HSLuv) -> Element.Color -> Element.Color
apply fn color =
    color |> uiToHsluv |> fn |> hsluvToUi


increment =
    0.05


veryLight : Element.Color -> Element.Color
veryLight color =
    apply (mapLightness ((+) (increment * 3))) color


light : Element.Color -> Element.Color
light color =
    apply (mapLightness ((+) (increment * 2))) color


lightish : Element.Color -> Element.Color
lightish color =
    apply (mapLightness ((+) (increment * 1))) color


darkish : Element.Color -> Element.Color
darkish color =
    apply (mapLightness (\l -> l - (increment * 1))) color


dark : Element.Color -> Element.Color
dark color =
    apply (mapLightness (\l -> l - (increment * 2))) color


veryDark : Element.Color -> Element.Color
veryDark color =
    apply (mapLightness (\l -> l - (increment * 3))) color


invert : Element.Color -> Element.Color
invert color =
    color |> apply (mapRed inv >> mapGreen inv >> mapBlue inv)


inv f =
    1 - f


desaturate : Element.Color -> Element.Color
desaturate color =
    color |> apply (setSaturation 0)
