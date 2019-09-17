module Chain exposing (Block, Chain, Link(..), Node, mockChain, view)

import Color exposing (..)
import Color.Interpolate exposing (..)
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Time exposing (..)


type alias Node =
    { name : String
    , id : String
    , currentBlock : String
    }


type alias Block =
    { hash : String
    , finalized : Maybe Posix
    }


type Link
    = Link Block
    | Fork (List Link)


type alias Chain =
    { chain : List Link
    , nodes : List Node
    }


mockChain =
    { chain = mockLinks
    , nodes = mockNodes
    }


mockLinks =
    [ Link { hash = "32ef", finalized = Just (millisToPosix <| 1568724381000 - 30000) }
    , Link { hash = "923a", finalized = Just (millisToPosix <| 1568724381000 - 20000) }
    , Link { hash = "d2c3", finalized = Just (millisToPosix <| 1568724381000 - 10000) }
    , Link { hash = "89fa", finalized = Just (millisToPosix 1568724381000) }
    , Link { hash = "2d45", finalized = Nothing }
    , Link { hash = "8574", finalized = Nothing }
    , Fork
        [ Link { hash = "235a", finalized = Nothing }
        , Link { hash = "352b", finalized = Nothing }
        ]
    ]


mockNodes : List Node
mockNodes =
    [ { name = "Node1", id = "0000000000000001", currentBlock = "32ef" }
    , { name = "Node2", id = "0000000000000002", currentBlock = "923a" }
    , { name = "Node3", id = "0000000000000003", currentBlock = "923a" }
    , { name = "Node4", id = "0000000000000004", currentBlock = "d2c3" }
    , { name = "Node5", id = "0000000000000005", currentBlock = "89fa" }
    , { name = "Node6", id = "0000000000000006", currentBlock = "89fa" }
    , { name = "Node7", id = "0000000000000007", currentBlock = "89fa" }
    , { name = "Node8", id = "0000000000000008", currentBlock = "2d45" }
    , { name = "Node9", id = "0000000000000009", currentBlock = "8574" }
    ]


view : Maybe Chain -> Chain -> Float -> Element msg
view lastState currentState transitionTime =
    row [ spacingXY 40 0, centerX, centerY ]
        (List.map viewLink currentState.chain)


viewLink : Link -> Element msg
viewLink link =
    case link of
        Link block ->
            el
                [ Background.color (blockBackground block)
                , Font.color (blockColor block)
                , Border.rounded 5
                , Font.size 16
                , width (px 80)
                , height (px 44)
                ]
                (el [ centerX, centerY ] (text block.hash))

        Fork candidates ->
            viewFork candidates


viewFork : List Link -> Element msg
viewFork candidates =
    column [ spacingXY 0 20 ] (List.map viewLink candidates)


blockColor : Block -> Element.Color
blockColor block =
    case block.finalized of
        Just posix ->
            toUI <| Colors.green

        Nothing ->
            toUI <| Colors.blue


blockBackground : Block -> Element.Color
blockBackground block =
    toUI <|
        interpolate LAB (fromUI <| blockColor block) Colors.blueishBlack 0.7
