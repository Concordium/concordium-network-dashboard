module Palette exposing
    ( ColorMode(..)
    , Palette
    , colorToUI
    , dark
    , darkish
    , defaultDark
    , defaultLight
    , desaturate
    , invert
    , light
    , lightish
    , mapPalette
    , toHex
    , uiToColor
    , uiToHsluv
    , veryDark
    , veryLight
    , withAlphaCo
    , withAlphaEl
    )

import Color exposing (Color, rgb255)
import Color.Convert exposing (colorToHex, hexToColor)
import Element
import HSLuv exposing (..)
import HSLuv.Manipulate exposing (..)


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
    , fg2 = lightGray
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
    { bg1 = veryLightGray
    , bg2 = white
    , bg3 = lightBrown
    , fg1 = fromHex "#181817"
    , fg2 = darkBrown
    , fg3 = blueishVeryDarkGray
    , c1 = darkish lightBlueNew
    , c2 = fromHex "#33A61E"
    , c3 = darkish purple
    , c4 = dark pink
    , c5 = brown
    , success = fromHex "#33A61E"
    , failure = red
    , danger = orange
    , warning = yellow
    , info = fromHex "#A0A0A0"
    , deactivated = dark blueishGray
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


veryLightGray =
    fromHex "#f4f4f1"



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
    fromHex "#646C78"


blueishDarkGray =
    fromHex "#152030"


blueishGray =
    fromHex "#3D4657"


midGray =
    fromHex "#666677"


lightGray =
    fromHex "#9999aa"


nearWhite =
    fromHex "#dddddd"


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
colorToUI color =
    Element.fromRgb (Color.toRgba color)


uiToColor : Element.Color -> Color
uiToColor color =
    Color.fromRgba (Element.toRgb color)


hsluvToUi : HSLuv -> Element.Color
hsluvToUi =
    Element.fromRgb << HSLuv.toRgba


uiToHsluv : Element.Color -> HSLuv
uiToHsluv =
    HSLuv.rgba << Element.toRgb



-- Modification


withAlphaEl : Float -> Element.Color -> Element.Color
withAlphaEl alpha color =
    let
        crgb =
            Element.toRgb color
    in
    Element.fromRgb { crgb | alpha = alpha }


withAlphaCo : Float -> Color -> Color
withAlphaCo alpha color =
    let
        colorRgba =
            Color.toRgba color
    in
    Color.fromRgba { colorRgba | alpha = alpha }


apply : (HSLuv -> HSLuv) -> Element.Color -> Element.Color
apply fn color =
    color |> uiToHsluv |> fn |> hsluvToUi


increment =
    0.02


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


inv : number -> number
inv f =
    1 - f


desaturate : Element.Color -> Element.Color
desaturate color =
    color |> apply (setSaturation 0)
