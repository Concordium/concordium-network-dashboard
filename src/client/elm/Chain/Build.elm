module Chain.Build exposing (..)

import File exposing (File)
import File.Download as Download
import File.Select as Select
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Tree exposing (Tree(..), singleton, tree)
import Tree.Zipper as Zipper exposing (Zipper)


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
    , forkWidth : Int
    , blockHeight : Int
    }


type BlockStatus
    = Finalized
    | LastFinalized
    | Candidate



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


getNodeInfo : String -> (WebData (List Node) -> msg) -> Cmd msg
getNodeInfo collectorEndpoint responseMsg =
    Http.get
        { url = collectorEndpoint ++ "/nodesBlocksInfo"
        , expect = Http.expectJson (RemoteData.fromResult >> responseMsg) (Decode.list decodeNode)
        }



-- Decoders / Encoders


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


{-| Convert a tree of "proto blocks" into a tree of blocks which know about their finalization state
and nodes that report them as their best block.
-}
annotate : List Node -> String -> Tree ProtoBlock -> Tree Block
annotate nodes lastFinalizedBlock sourceTree =
    annotateChain nodes sourceTree
        |> colorize lastFinalizedBlock


annotateChain : List Node -> Tree ProtoBlock -> Tree Block
annotateChain nodes sourceTree =
    Tree.restructure
        (annotateBlock nodes)
        annotateChildren
        sourceTree


annotateBlock : List Node -> ProtoBlock -> Block
annotateBlock nodes ( height, hash ) =
    let
        nodesAt =
            nodes
                |> List.filter (\node -> node.bestBlock == hash)
                |> List.map .nodeId

        fractionNodesAt =
            toFloat (List.length nodesAt) / toFloat (List.length nodes)
    in
    { hash = hash
    , nodesAt = nodesAt
    , fractionNodesAt = fractionNodesAt
    , status = Candidate
    , forkWidth = 1
    , blockHeight = height
    }


annotateChildren : Block -> List (Tree Block) -> Tree Block
annotateChildren label children =
    case children of
        [] ->
            singleton label

        _ ->
            tree
                { label
                    | forkWidth =
                        children
                            |> List.map (Tree.label >> .forkWidth)
                            |> List.sum
                }
                (List.sortBy
                    (Tree.map .fractionNodesAt >> Tree.flatten >> List.sum >> (*) -1)
                    children
                )


{-| Starting from the last finalized block, mark all ancestor blocks as finalized.
-}
colorize : String -> Tree Block -> Tree Block
colorize lastFinalizedBlock tree =
    let
        startZipper =
            tree
                |> Zipper.fromTree
                |> Zipper.findFromRoot
                    (.hash >> (==) lastFinalizedBlock)

        colorizeUp zipper =
            let
                coloredZipper =
                    Zipper.mapLabel (\label -> { label | status = Finalized }) zipper
            in
            case Zipper.parent coloredZipper of
                Nothing ->
                    coloredZipper

                Just parent ->
                    colorizeUp parent
    in
    Maybe.map colorizeUp startZipper
        |> Maybe.map Zipper.toTree
        |> Maybe.withDefault tree
