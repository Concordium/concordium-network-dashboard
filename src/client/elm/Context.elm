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
        }


type alias Theme a =
    { a
        | palette : Palette Color
        , colorMode : Palette.ColorMode
    }
