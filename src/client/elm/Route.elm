module Route exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Dashboard
    | NodeView String -- Node by nodeId
    | ChainInit
    | ChainSelected String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Dashboard (s "")
        , map NodeView (s "node" </> string)
        , map ChainInit (s "chain")
        , map ChainSelected (s "chain" </> string)
        ]


fromUrl : Url -> Route
fromUrl url =
    case parse parser url of
        Just page ->
            page

        Nothing ->
            Dashboard


toString : Route -> String
toString route =
    case route of
        Dashboard ->
            "/"

        NodeView nodeId ->
            "/node/" ++ nodeId

        ChainInit ->
            "/chain"

        ChainSelected hash ->
            "/chain/" ++ hash
