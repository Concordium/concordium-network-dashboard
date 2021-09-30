module Chain.Build exposing (..)

import File exposing (File)
import File.Download as Download
import File.Select as Select
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import RemoteData exposing (WebData, update)
import Tree exposing (Tree(..), singleton, tree)


type alias Node =
    { nodeName : String
    , nodeId : String
    , bestBlock : String
    , bestBlockHeight : Int
    , finalizedBlock : String
    , finalizedBlockHeight : Int
    , finalizedBlockParent : String
    , ancestorsSinceBestBlock : List String
    }


{-| The first description of a block, consisting of block height and hash.
-}
type alias ProtoBlock =
    ( Int, String )


type alias Block =
    { hash : String
    , nodesAt : List String
    , fractionNodesAt : Float
    , status : BlockStatus
    , blockHeight : Int
    , branchPosition : Int
    }


type BlockStatus
    = Finalized
    | LastFinalized
    | Candidate
    | Discarded



-- Saving and Loading History from Files (mainly for debugging)


type alias Replay =
    { present : List Node
    , past : List (List Node)
    , future : List (List Node)
    }


advanceReplay : Replay -> Replay
advanceReplay replay =
    case replay.future of
        [] ->
            replay

        nearFuture :: farFuture ->
            { present = nearFuture
            , past = replay.present :: replay.past
            , future = farFuture
            }


saveNodeHistory : List (List Node) -> Cmd msg
saveNodeHistory history =
    history
        |> (Encode.list <| Encode.list <| encodeNode)
        |> Encode.encode 0
        |> Download.string "history.json" "application/json"


loadNodeHistory : (File -> msg) -> Cmd msg
loadNodeHistory loadedMsg =
    Select.file [ "application/json" ] loadedMsg


decodeHistory : Decode.Decoder (List (List Node))
decodeHistory =
    Decode.list <| Decode.list <| decodeNode



-- Requests


getNodeInfo : (WebData (List Node) -> msg) -> Cmd msg
getNodeInfo responseMsg =
    Http.get
        { url = "/nodesBlocksInfo"
        , expect = Http.expectJson (RemoteData.fromResult >> responseMsg) (Decode.list decodeNode)
        }


getBlockByHeight : Int -> (WebData String -> msg) -> Cmd msg
getBlockByHeight height responseMsg =
    Http.get
        { url = "/v1/blocksByHeight/" ++ String.fromInt height
        , expect = Http.expectJson (RemoteData.fromResult >> responseMsg) decodeBlockHash
        }



-- Decoders / Encoders


decodeBlockHash : Decode.Decoder String
decodeBlockHash =
    Decode.index 0 Decode.string


decodeNode : Decode.Decoder Node
decodeNode =
    Decode.succeed Node
        |> Decode.required "nodeName" Decode.string
        |> Decode.required "nodeId" Decode.string
        |> Decode.required "bestBlock" Decode.string
        |> Decode.required "bestBlockHeight" Decode.int
        |> Decode.required "finalizedBlock" Decode.string
        |> Decode.required "finalizedBlockHeight" Decode.int
        |> Decode.required "finalizedBlockParent" Decode.string
        |> Decode.required "ancestorsSinceBestBlock"
            (Decode.oneOf
                [ Decode.list Decode.string
                , Decode.succeed []
                ]
            )


encodeNode : Node -> Encode.Value
encodeNode record =
    Encode.object
        [ ( "nodeName", Encode.string <| record.nodeName )
        , ( "nodeId", Encode.string <| record.nodeId )
        , ( "bestBlock", Encode.string <| record.bestBlock )
        , ( "bestBlockHeight", Encode.int <| record.bestBlockHeight )
        , ( "finalizedBlock", Encode.string <| record.finalizedBlock )
        , ( "finalizedBlockHeight", Encode.int <| record.finalizedBlockHeight )
        , ( "finalizedBlockParent", Encode.string <| record.finalizedBlockParent )
        , ( "ancestorsSinceBestBlock"
          , Encode.list Encode.string record.ancestorsSinceBestBlock
          )
        ]



-- Build the chain


{-| Construct sequence of proto blocks from node.
-}
prepareBlockSequence : Node -> List ProtoBlock
prepareBlockSequence node =
    let
        seq =
            ( node.finalizedBlockHeight, node.finalizedBlock )
                :: (node.ancestorsSinceBestBlock
                        |> List.reverse
                        |> List.indexedMap
                            (\index hash ->
                                ( node.finalizedBlockHeight + (index + 1), hash )
                            )
                   )
    in
    if node.finalizedBlockHeight == 0 then
        seq

    else
        ( node.finalizedBlockHeight - 1, node.finalizedBlockParent ) :: seq



-- Annotating the chain to prepare for viewing


{-| Convert a tree of "proto blocks" into a tree of blocks which know about their finalization state,
nodes that report them as their best block and their positioning for drawing.
-}
annotate : List Node -> Int -> Tree ProtoBlock -> Tree Block
annotate nodes lastFinalizedHeight sourceTree =
    annotateChain nodes lastFinalizedHeight sourceTree
        |> updateDiscardedStatus


annotateChain : List Node -> Int -> Tree ProtoBlock -> Tree Block
annotateChain nodes lastFinalizedHeight sourceTree =
    Tree.restructure
        (annotateBlock nodes lastFinalizedHeight)
        annotateChildren
        sourceTree


annotateBlock : List Node -> Int -> ProtoBlock -> Block
annotateBlock nodes lastFinalizedHeight ( height, hash ) =
    let
        nodesAt =
            nodes
                |> List.filter (\node -> node.bestBlock == hash)
                |> List.map .nodeId

        fractionNodesAt =
            if List.length nodes == 0 then
                0

            else
                toFloat (List.length nodesAt) / toFloat (List.length nodes)
    in
    { hash = hash
    , nodesAt = nodesAt
    , fractionNodesAt = fractionNodesAt
    , status = statusFromHeight lastFinalizedHeight height
    , blockHeight = height
    , branchPosition = 0
    }


annotateChildren : Block -> List (Tree Block) -> Tree Block
annotateChildren label children =
    case children of
        [] ->
            singleton label

        _ ->
            let
                sortedChildren =
                    List.sortBy subtreeWeighting children

                positionedChildren =
                    calculateBranchPosition sortedChildren
            in
            tree
                label
                positionedChildren


{-| Folds through a list of child branches, assigning each branch a vertical position based
on the layout of the previous branch. By taking the length of the current branch into account,
branches are packed more space-efficiently than with the naive approach.
-}
calculateBranchPosition : List (Tree Block) -> List (Tree Block)
calculateBranchPosition children =
    children
        |> List.foldl
            (\child placedChildren ->
                let
                    -- calculate the length of the current branch
                    maxHeight =
                        child |> Tree.map .blockHeight |> Tree.foldl max 1

                    -- calculate the heighest vertical position of the previous branches up to maxHeight
                    previousMaxBranchPosition =
                        List.head placedChildren
                            |> Maybe.map
                                (Tree.flatten
                                    >> List.filter (\label -> label.blockHeight <= maxHeight)
                                    >> List.map .branchPosition
                                    >> List.foldl max 0
                                    >> (+) 1
                                )
                            |> Maybe.withDefault 0

                    -- update all blocks in the child branch
                    updateLabel label =
                        { label | branchPosition = label.branchPosition + previousMaxBranchPosition }
                in
                Tree.map updateLabel child :: placedChildren
            )
            []


{-| Called on the root element, this recurses through a tree, marking branches that start from a finalized block as discarded.
-}
updateDiscardedStatus : Tree Block -> Tree Block
updateDiscardedStatus tree =
    Tree.mapChildren (List.map (updateDiscardedStatusWithParent (Tree.label tree))) tree


{-| Helper function for doing the recursion in updateDiscardedStatus
-}
updateDiscardedStatusWithParent : Block -> Tree Block -> Tree Block
updateDiscardedStatusWithParent parent tree =
    let
        label =
            Tree.label tree

        updatedLabel =
            { label
                | status =
                    if label.branchPosition > 0 && (parent.status == Finalized || parent.status == Discarded) then
                        Discarded

                    else
                        label.status
            }

        updatedChildren =
            Tree.children tree |> List.map (updateDiscardedStatusWithParent updatedLabel)
    in
    Tree.tree updatedLabel updatedChildren


{-| Calculates a weighting for sorting subtrees that prioritizes the tree containing
the block that most nodes consider `bestBlock`. If there is a draw, the total weight
of the subtree is considered.
-}
subtreeWeighting : Tree Block -> Float
subtreeWeighting tree =
    let
        weights =
            tree
                |> Tree.map .fractionNodesAt
                |> Tree.flatten

        maxWeight =
            weights
                |> List.maximum
                |> Maybe.withDefault 0

        weight =
            10 * maxWeight + List.sum weights
    in
    -- Negate weight to do descending sort.
    -weight


{-| Calculate the initial status of a block based on its height and the height of the last finalized block
This does not take into account discarded blocks, which are marked as such after the Y position is calculated
-}
statusFromHeight : Int -> Int -> BlockStatus
statusFromHeight lastFinalizedHeight height =
    case compare height lastFinalizedHeight of
        LT ->
            Finalized

        EQ ->
            LastFinalized

        GT ->
            Candidate
