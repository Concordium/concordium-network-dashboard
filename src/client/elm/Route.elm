module Route exposing (Route(..), fromUrl, isChain, toString)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Network
    | NodeView String -- Node by nodeId
    | ChainInit
    | ChainSelected String
    | Lookup
    | LookupTransaction String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Network (s "")
        , map NodeView (s "node" </> string)
        , map ChainInit (s "chain")
        , map ChainSelected (s "chain" </> string)
        , map Lookup (s "lookup")
        , map LookupTransaction (s "lookup" </> string)
        ]


fromUrl : Url -> Route
fromUrl url =
    case parse parser url of
        Just page ->
            page

        Nothing ->
            Network


toString : Route -> String
toString route =
    case route of
        Network ->
            "/"

        NodeView nodeId ->
            "/node/" ++ nodeId

        ChainInit ->
            "/chain"

        ChainSelected hash ->
            "/chain/" ++ hash

        Lookup ->
            "/lookup"

        LookupTransaction txHash ->
            "/lookup/" ++ txHash


isChain : Route -> Bool
isChain route =
    case route of
        ChainInit ->
            True

        ChainSelected _ ->
            True

        _ ->
            False
