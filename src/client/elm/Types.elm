module Types exposing (..)

import Browser exposing (..)
import Browser.Navigation exposing (Key)
import Chain
import Element
import Explorer
import Json.Decode as D
import Network
import Palette exposing (ColorMode, Palette)
import Route exposing (Route)
import Time
import Url exposing (Url)


type alias Model =
    { key : Key
    , time : Time.Posix
    , window : { width : Int, height : Int }
    , palette : Palette Element.Color
    , colorMode : ColorMode
    , currentRoute : Route
    , networkModel : Network.Model
    , chainModel : Chain.Model
    , explorerModel : Explorer.Model
    }


type Msg
    = CurrentTime Time.Posix
    | UrlClicked UrlRequest
    | UrlChanged Url
    | WindowResized Int Int
    | CopyToClipboard String
    | StorageDocReceived D.Value
      --
    | NetworkMsg Network.Msg
    | ChainMsg Chain.Msg
    | BlockSelected String
    | ToggleDarkMode
    | ExplorerMsg Explorer.Msg
