module Types exposing (Flags, Model, Msg(..), Page(..), pageToPath, parserRoutes, pathToPage)

import Browser exposing (..)
import Browser.Navigation as Nav exposing (Key)
import Dict
import Enum
import Time
import Url exposing (Url)
import Url.Parser exposing (..)


type alias Flags =
    { width : Int, height : Int }


type alias Model =
    { currentTime : Time.Posix
    , window : { width : Int, height : Int }
    , key : Key
    , currentPage : Page
    }


type Msg
    = CurrentTime Time.Posix
    | UrlClicked UrlRequest
    | UrlChanged Url
    | OpenPage Page
    | OpenUrl String
    | WindowResized Int Int
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
