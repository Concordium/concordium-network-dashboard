module Chain exposing (Block, Chain, Node, mockChain, view)

import Chain.Connector exposing (..)
import Chain.Spec exposing (..)
import Color exposing (..)
import Color.Interpolate exposing (..)
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import List.Extra as List
import Time exposing (..)



-- Node


type alias Node =
    { name : String
    , id : String
    , currentBlock : String
    }



-- Chain


type alias Block =
    { hash : String
    }


type Chain
    = Chain (List Block) (List Chain)



-- Annotated Chain


type alias AnnotatedBlock =
    { hash : String
    , numNodesAt : Int
    , percentageNodesAt : Float
    , color : Element.Color
    , background : Element.Color
    , connectorsTo : List Int
    }


type AnnotatedChain
    = AnnotatedChain Int (List AnnotatedBlock) (List AnnotatedChain)



-- Deriving the annotations


chainHeight : AnnotatedChain -> Int
chainHeight chain =
    case chain of
        AnnotatedChain height blocks subChains ->
            height


annotateChain : Chain -> AnnotatedChain
annotateChain chain =
    case chain of
        Chain blocks [] ->
            AnnotatedChain 1 (annotateBlocks mockNodes blocks []) []

        Chain blocks subChains ->
            let
                annotatedSubChains =
                    List.map annotateChain subChains

                height =
                    List.map chainHeight annotatedSubChains
                        |> List.sum
            in
            AnnotatedChain
                height
                (annotateBlocks mockNodes blocks (connectorsTo annotatedSubChains))
                annotatedSubChains


annotateBlocks : List Node -> List Block -> List Int -> List AnnotatedBlock
annotateBlocks nodes blocks connectorsLast =
    case blocks of
        [] ->
            []

        lastBlock :: [] ->
            [ annotateBlock nodes lastBlock connectorsLast ]

        currentBlock :: remainingBlocks ->
            annotateBlock nodes currentBlock [ 0 ]
                :: annotateBlocks nodes remainingBlocks connectorsLast


annotateBlock : List Node -> Block -> List Int -> AnnotatedBlock
annotateBlock nodes block connectors =
    let
        numNodes =
            List.length nodes

        numNodesAt =
            nodesAt nodes block.hash
    in
    { hash = block.hash
    , numNodesAt = nodesAt nodes block.hash
    , percentageNodesAt = (numNodesAt |> toFloat) / (numNodes |> toFloat)
    , color = blockColor Candidate
    , background = blockBackground Candidate
    , connectorsTo = connectors
    }


nodesAt : List Node -> String -> Int
nodesAt nodes hash =
    nodes
        |> List.filter (\node -> node.currentBlock == hash)
        |> List.length


connectorsTo : List AnnotatedChain -> List Int
connectorsTo chains =
    List.map chainHeight chains
        |> List.scanl (+) 0
        |> List.unconsLast
        |> Maybe.withDefault ( 0, [] )
        |> Tuple.second



-- Mock Data


mockChain =
    Chain [ { hash = "ddf3" }, { hash = "34ef" } ]
        [ Chain [ { hash = "32ef" }, { hash = "89fa" }, { hash = "hallo" } ]
            [ Chain [ { hash = "2aa6" }, { hash = "32ef" } ] [ Chain [ { hash = "2aa6" }, { hash = "32ef" } ] [], Chain [ { hash = "2aa6" }, { hash = "32ef" } ] [] ]
            , Chain [ { hash = "32ef" }, { hash = "32ef" } ] []
            , Chain [ { hash = "32ef" }, { hash = "32ef" } ] [ Chain [ { hash = "32ef" }, { hash = "89fa" } ] [ Chain [ { hash = "32ef" }, { hash = "89fa" } ] [] ] ]
            , Chain [ { hash = "32ef" }, { hash = "32ef" } ] []
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



-- View


viewAnnotatedChain : Spec -> AnnotatedChain -> Element msg
viewAnnotatedChain spec chain =
    case chain of
        AnnotatedChain n blocks [] ->
            viewAnnotatedBlocks blocks

        AnnotatedChain n blocks subChains ->
            row [ alignTop ]
                [ viewAnnotatedBlocks blocks
                , column [ alignTop, spacing (round spec.gutterHeight) ]
                    (List.map (viewAnnotatedChain spec) subChains)
                ]


viewAnnotatedBlocks : List AnnotatedBlock -> Element msg
viewAnnotatedBlocks blocks =
    row [ alignTop ] (List.map viewBlock blocks)


type BlockStatus
    = Finalized
    | LastFinalized
    | Candidate


view : Chain -> Element msg
view chain =
    row [ centerX, centerY ]
        [ viewAnnotatedChain spec (annotateChain chain)
        ]


viewBlock : AnnotatedBlock -> Element msg
viewBlock block =
    row [ alignTop ]
        [ column [ spacing 4, alignTop ]
            [ el
                [ height (px 6)
                , width (px <| round (toFloat 64 * block.percentageNodesAt))
                , Border.rounded 10
                , Background.color (toUI Colors.purple)
                , alignTop
                ]
                none
            , el
                [ Background.color block.background
                , Font.color block.color
                , Border.rounded 3
                , Font.size 16
                , width (px 64)
                , height (px 36)
                , alignTop
                ]
                (el [ centerX, centerY ] (text block.hash))
            ]
        , connector spec (fromUI block.background) block.connectorsTo
        ]


viewSummaryBlock : Int -> Element msg
viewSummaryBlock n =
    el
        [ Font.color (blockBackground Candidate)
        , Border.rounded 3
        , Border.dotted
        , Border.color (blockBackground Candidate)
        , Border.width 2
        , Font.size 16
        , width (px 64)
        , height (px 36)
        ]
        (el [ centerX, centerY ] (text ("+" ++ String.fromInt n)))


blockColor : BlockStatus -> Element.Color
blockColor status =
    case status of
        Finalized ->
            toUI <| Colors.green

        LastFinalized ->
            toUI <| Colors.green

        Candidate ->
            toUI <| Colors.blue


blockBackground : BlockStatus -> Element.Color
blockBackground status =
    let
        bgAlpha =
            case status of
                LastFinalized ->
                    0.5

                _ ->
                    0.75
    in
    toUI <|
        interpolate LAB (fromUI <| blockColor status) Colors.blueishBlack bgAlpha
