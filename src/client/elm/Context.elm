module Context exposing (..)

import Browser.Navigation exposing (Key)
import Clipboard
import Element exposing (Color)
import Palette exposing (Palette)
import Time exposing (Posix)


type alias Window =
    { width : Int, height : Int }


type alias Context a =
    { a
        | key : Key
        , time : Posix
        , window : Window
        , palette : Palette Color
        , colorMode : Palette.ColorMode
    }


type GlobalMsg
    = CopyToClipboard String


type Msg localMsg
    = Global GlobalMsg
    | Local localMsg


update : GlobalMsg -> Cmd msg
update msg =
    case msg of
        CopyToClipboard str ->
            Clipboard.copy str


copyToClipboard : String -> Msg localMsg
copyToClipboard =
    Global << CopyToClipboard


translate : (GlobalMsg -> msg) -> (localMsg -> msg) -> Msg localMsg -> msg
translate onGlobalMsg onLocalMsg msg =
    case msg of
        Global globalMsg ->
            onGlobalMsg globalMsg

        Local localMsg ->
            onLocalMsg localMsg
