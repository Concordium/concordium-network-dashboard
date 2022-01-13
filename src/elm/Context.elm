module Context exposing (..)

import Browser.Navigation exposing (Key)
import Element exposing (Color)
import Palette exposing (Palette)
import Time exposing (Posix)


type alias Window =
    { width : Int, height : Int }


type alias Context a =
    Theme
        { a
            | key : Key
            , time : Posix
            , window : Window
            , statsVersion : String
        }


type alias Theme a =
    { a
        | palette : Palette Color
        , colorMode : Palette.ColorMode
    }


{-| Strip out every field except the theme related ones
-}
extractTheme : Theme a -> Theme {}
extractTheme ctx =
    { palette = ctx.palette, colorMode = ctx.colorMode }
