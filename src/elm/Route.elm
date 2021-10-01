module Route exposing (Route(..), fromUrl, isChain, isLookup, isNetwork, toString)

import Types exposing (BlockHash, TxHash)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Network
    | NodeView String -- Node by nodeId
    | Chain (Maybe BlockHash)
    | Lookup (Maybe TxHash)


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Network (Parser.s "")
        , Parser.map NodeView (Parser.s "node" </> Parser.string)
        , Parser.map (Chain Nothing) (Parser.s "chain")
        , Parser.map (Chain << Just) (Parser.s "chain" </> Parser.string)
        , Parser.map (Lookup Nothing) (Parser.s "lookup")
        , Parser.map (Lookup << Just) (Parser.s "lookup" </> Parser.string)
        ]


fromUrl : Url -> Route
fromUrl url =
    case Parser.parse parser url of
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

        Chain Nothing ->
            "/chain"

        Chain (Just hash) ->
            "/chain/" ++ hash

        Lookup Nothing ->
            "/lookup"

        Lookup (Just txHash) ->
            "/lookup/" ++ txHash


isChain : Route -> Bool
isChain route =
    case route of
        Chain _ ->
            True

        _ ->
            False


isLookup : Route -> Bool
isLookup route =
    case route of
        Lookup _ ->
            True

        _ ->
            False


isNetwork : Route -> Bool
isNetwork route =
    case route of
        Network ->
            True

        _ ->
            False
