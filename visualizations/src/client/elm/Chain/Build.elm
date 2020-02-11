module Chain.Build exposing (..)

import Chain.DictTree as DictTree
import Dict as Dict exposing (Dict)
import Dict.Extra as Dict
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import List.Extra as List
import RemoteData exposing (WebData)
import Tree exposing (Tree(..), singleton, tree)
import Tree.Zipper as Zipper exposing (Zipper)


nodeInfoEndpoint : String
nodeInfoEndpoint =
    "https://dashboard.eu.prod.concordium.com/nodesBlocksInfo"


type alias Node =
    { nodeName : String
    , nodeId : String
    , bestBlock : String
    , bestBlockHeight : Int
    , finalizedBlock : String
    , finalizedBlockHeight : Int
    , ancestorsSinceBestBlock : List String
    }


{-| The first description of a block, consisting of Block Height and hash.
-}
type alias ProtoBlock =
    ( Int, String )


type alias Block =
    { hash : String
    , shortHash : String
    , nodesAt : List String
    , numNodesAt : Int
    , numNodesAtTree : Int
    , connectors : List Int
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



-- Mock Tree for testing


mockTree =
    DictTree.init
        |> DictTree.addAll
            [ [ ( 0, "a1" ), ( 0, "b1" ), ( 0, "c1" ), ( 0, "d1" ), ( 0, "e1" ) ]
            , [ ( 0, "b1" ), ( 0, "c2" ), ( 0, "d2" ) ]
            , [ ( 0, "c1" ), ( 0, "x" ), ( 0, "y" ) ]
            , [ ( 0, "d1" ), ( 0, "y2" ) ]
            ]
        |> (\dtree ->
                DictTree.buildForward
                    10
                    ( 0, "a1" )
                    dtree
                    []
                    Tree.tree
           )
        |> annotate mockNodes ( 0, "a1" )


mockNodes =
    [ { nodeName = "MockNode"
      , nodeId = "MockNode"
      , bestBlock = "e1"
      , bestBlockHeight = 0
      , finalizedBlock = "a1"
      , finalizedBlockHeight = 0
      , ancestorsSinceBestBlock = [ "d2" ]
      }
    ]



-- Requests


getNodeInfo : (WebData (List Node) -> msg) -> Cmd msg
getNodeInfo responseMsg =
    Http.get
        { url = nodeInfoEndpoint
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
        |> Decode.required "ancestorsSinceBestBlock" (Decode.list Decode.string)


encodeNode : Node -> Encode.Value
encodeNode record =
    Encode.object
        [ ( "nodeName", Encode.string <| record.nodeName )
        , ( "nodeId", Encode.string <| record.nodeId )
        , ( "bestBlock", Encode.string <| record.bestBlock )
        , ( "bestBlockHeight", Encode.int <| record.bestBlockHeight )
        , ( "finalizedBlock", Encode.string <| record.finalizedBlock )
        , ( "finalizedBlockHeight", Encode.int <| record.finalizedBlockHeight )
        , ( "ancestorsSinceBestBlock"
          , Encode.list Encode.string record.ancestorsSinceBestBlock
          )
        ]



-- Build the chain


prepareBlockSequence : Node -> List ProtoBlock
prepareBlockSequence node =
    ( node.finalizedBlockHeight, node.finalizedBlock )
        :: (node.ancestorsSinceBestBlock
                |> List.reverse
                |> List.indexedMap
                    (\index hash ->
                        ( node.finalizedBlockHeight + (index + 1), hash )
                    )
           )



-- Annotating the chain to prepare for viewing


annotate : List Node -> ProtoBlock -> Tree ProtoBlock -> Tree Block
annotate nodes lastFinalized sourceTree =
    annotateChain nodes sourceTree
        |> colorize lastFinalized


annotateChain : List Node -> Tree ProtoBlock -> Tree Block
annotateChain nodes sourceTree =
    Tree.restructure
        (annotateBlock nodes)
        annotateChildren
        sourceTree


annotateBlock : List Node -> ProtoBlock -> Block
annotateBlock nodes =
    block nodes 1 ( 0, 0 ) []


annotateChildren : Block -> List (Tree Block) -> Tree Block
annotateChildren label children =
    case children of
        [] ->
            singleton label

        _ ->
            children
                |> List.sortBy (Tree.label >> .numNodesAtTree >> (*) -1)
                |> (\sortedChildren ->
                        tree
                            { label
                                | forkWidth =
                                    sortedChildren
                                        |> List.map (Tree.label >> .forkWidth)
                                        |> List.sum
                                , numNodesAtTree =
                                    sortedChildren
                                        |> List.map (Tree.label >> .numNodesAt)
                                        |> List.sum
                                , connectors = connectors sortedChildren
                            }
                            sortedChildren
                   )


colorize : ProtoBlock -> Tree Block -> Tree Block
colorize lastFinalized tree =
    let
        startZipper =
            tree
                |> Zipper.fromTree
                |> Zipper.findFromRoot
                    (.hash >> (==) (Tuple.second lastFinalized))

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


connectors : List (Tree Block) -> List Int
connectors branches =
    List.map (Tree.label >> .forkWidth) branches
        |> List.scanl (+) 0
        |> List.unconsLast
        |> Maybe.withDefault ( 0, [] )
        |> Tuple.second


block : List Node -> Int -> ( Int, Int ) -> List Int -> ProtoBlock -> Block
block nodes forkWidth position connectorList ( height, hash ) =
    let
        nodesAt =
            nodes
                |> List.filter (\node -> node.bestBlock == hash)
                |> List.map .nodeId

        numNodesAt =
            List.length nodesAt
    in
    { hash = hash
    , shortHash = String.left 4 hash
    , nodesAt = nodesAt
    , numNodesAt = numNodesAt
    , numNodesAtTree = numNodesAt
    , connectors = connectorList
    , status = Candidate
    , forkWidth = forkWidth
    , blockHeight = height
    }
