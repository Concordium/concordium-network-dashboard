module Types exposing (Flags, Model, Msg(..), Page(..), Window, pageToPath, parserRoutes, pathToPage)

import Animation exposing (Animation)
import Browser exposing (..)
import Browser.Navigation as Nav exposing (Key)
import Chain exposing (..)
import Dict
import Enum
import Graph exposing (Graph)
import RewardGraph exposing (..)
import Time
import Url exposing (Url)
import Url.Parser exposing (..)


type alias Flags =
    { width : Int, height : Int }


type alias Window =
    { width : Int, height : Int }


type alias Model =
    { currentTime : Time.Posix
    , window : Window
    , key : Key
    , currentPage : Page
    , selectedNode : Maybe Int
    , graph : Graph NodeSpec EdgeSpec
    , clock : Float
    , transfer : Animation
    , ticks : Int
    , chainModel : Chain.Model
    }


type Msg
    = CurrentTime Time.Posix
    | UrlClicked UrlRequest
    | UrlChanged Url
    | OpenPage Page
    | OpenUrl String
    | WindowResized Int Int
    | Tick Float
    | NodeHovered (Maybe Int)
    | EdgeValueChanged Int Int String
    | EdgeIntervalChanged Int Int String
    | ChainMsg Chain.Msg
    | Noop


type Page
    = Home


parserRoutes =
    oneOf
        [ map Home (s "")

        -- , map SomeOtherPage (s "some" </> string)
        ]


pathToPage : Url -> Page
pathToPage url =
    case parse parserRoutes url of
        Just page ->
            page

        Nothing ->
            Home


pageToPath : Page -> String
pageToPath page =
    case page of
        Home ->
            "/"
