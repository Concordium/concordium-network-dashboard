module Page exposing (Page(..), pageToPath, parserRoutes, pathToPage)

import Url exposing (Url)
import Url.Parser exposing (..)


type Page
    = Home


parserRoutes =
    oneOf
        [ map Home (s "")
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
