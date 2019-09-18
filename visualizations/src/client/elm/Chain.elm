module Chain exposing (Block, BlockChain, Node, mockChain, view)

import Chain.Connector exposing (..)
import Chain.Spec exposing (..)
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
    , nodesAt : List Node
    }


type alias Model =
    { chain : BlockChain
    , nodes : List Node
    }


type alias FinalizedChain =
    List Block


type CandidateTree
    = Chain (List Block) (List CandidateTree)


type alias BlockChain =
    { finalizedBlocks : FinalizedChain
    , candidateBlocks : CandidateTree
    }


mockModel =
    { chain = mockChain
    , nodes = mockNodes
    }


mockChain =
    { finalizedBlocks = mockFinalizedBlocks
    , candidateBlocks = mockCandidateBlocks
    }


mockFinalizedBlocks =
    [ { hash = "32ef", nodesAt = [] }
    , { hash = "923a", nodesAt = [] }
    , { hash = "d2c3", nodesAt = [] }
    ]


mockCandidateBlocks =
    Chain
        [ { hash = "32ef", nodesAt = [] }
        , { hash = "923a", nodesAt = [] }
        ]
        [ Chain [ { hash = "32ef", nodesAt = [] }, { hash = "89fa", nodesAt = [] } ] []
        , Chain [ { hash = "32ef", nodesAt = [] }, { hash = "32ef", nodesAt = [] } ]
            [ Chain [ { hash = "32ef", nodesAt = [] }, { hash = "32ef", nodesAt = [] } ] []
            , Chain [ { hash = "32ef", nodesAt = [] }, { hash = "32ef", nodesAt = [] } ] []
            ]
        , Chain [ { hash = "32ef", nodesAt = [] }, { hash = "32ef", nodesAt = [] } ] []
        , Chain [ { hash = "32ef", nodesAt = [] }, { hash = "32ef", nodesAt = [] } ] []
        , Chain [ { hash = "32ef", nodesAt = [] }, { hash = "32ef", nodesAt = [] } ] []
        , Chain [ { hash = "32ef", nodesAt = [] }, { hash = "32ef", nodesAt = [] } ] []
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


view : Maybe BlockChain -> BlockChain -> Float -> Element msg
view lastState currentState transitionTime =
    let
        viewFinalizedChain =
            viewChain True
    in
    row [ centerX, centerY ]
        [ viewFinalizedChain currentState.finalizedBlocks
        , connector spec (fromUI <| blockBackground Candidate) 1
        , viewCandidateTree 1 currentState.candidateBlocks
        ]


viewChain : Bool -> List Block -> Element msg
viewChain finalized blocks =
    row [ alignTop ]
        (if finalized then
            List.map (viewBlock Finalized) (List.take (List.length blocks - 1) blocks)
                ++ List.map (viewBlock LastFinalized) (List.drop (List.length blocks - 1) blocks)
                |> List.intersperse (connector spec (fromUI <| blockBackground Finalized) 1)

         else
            List.map (viewBlock Candidate) blocks
                |> List.intersperse (connector spec (fromUI <| blockBackground Candidate) 1)
        )


type BlockStatus
    = Finalized
    | LastFinalized
    | Candidate


viewBlock : BlockStatus -> Block -> Element msg
viewBlock blockStatus block =
    el
        [ Background.color (blockBackground blockStatus)
        , Font.color (blockColor blockStatus)
        , Border.rounded 3
        , Font.size 16
        , width (px 64)
        , height (px 36)
        ]
        (el [ centerX, centerY ] (text block.hash))


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


viewCandidateTree : Int -> CandidateTree -> Element msg
viewCandidateTree depth tree =
    case ( depth, tree ) of
        ( 0, Chain chain [] ) ->
            viewChain False (List.take 1 chain)

        ( 0, Chain chain subChains ) ->
            viewChain False (List.take 1 chain)

        ( n, Chain chain [] ) ->
            viewChain False chain

        ( n, Chain chain subChains ) ->
            row [ alignTop ]
                [ viewChain False chain
                , connector spec (fromUI <| blockBackground Candidate) (min 3 (List.length subChains))
                , column
                    [ spacingXY 0 22 ]
                    ((subChains |> List.take 2 |> List.map (viewCandidateTree (n - 1)))
                        ++ [ viewSummaryBlock (List.length subChains - 2) ]
                    )
                ]


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
