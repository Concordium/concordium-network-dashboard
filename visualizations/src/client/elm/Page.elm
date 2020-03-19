module Page exposing (Page(..), pageToPath, parserRoutes, pathToPage)

import Animation exposing (Animation)
import Browser exposing (..)
import Browser.Navigation as Nav exposing (Key)
import Chain exposing (..)
import Dict
import Graph exposing (Graph)
import RewardGraph exposing (..)
import Time
import Url exposing (Url)
import Url.Parser exposing (..)


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
