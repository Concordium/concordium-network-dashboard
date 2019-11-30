module Chain.Api exposing (..)

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


type alias Block =
    { hash : String
    , shortHash : String
    , numNodesAt : Int
    , percentageNodesAt : Float
    , connectors : List Int
    , status : BlockStatus
    , height : Int
    }


type alias Positioned a =
    { a
        | x : Int
        , y : Int
    }


type BlockStatus
    = Finalized
    | LastFinalized
    | Candidate



-- Saving and Loading Histry from Files (mainly for debugging)


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


prepareBlockSequence : Node -> List String
prepareBlockSequence node =
    node.finalizedBlock
        :: List.reverse node.ancestorsSinceBestBlock



-- Annotating the chain to prepare for viewing


annotate : List Node -> String -> Tree String -> Tree Block
annotate nodes lastFinalized sourceTree =
    annotateChain nodes sourceTree
        |> colorize lastFinalized


annotateChain : List Node -> Tree String -> Tree Block
annotateChain nodes sourceTree =
    Tree.restructure
        (annotateBlock nodes)
        annotateChildren
        sourceTree


annotateBlock : List Node -> String -> Block
annotateBlock nodes =
    block nodes 1 ( 0, 0 ) []


annotateChildren : Block -> List (Tree Block) -> Tree Block
annotateChildren label children =
    case children of
        [] ->
            singleton label

        _ ->
            let
                height =
                    children
                        |> List.map (Tree.label >> .height)
                        |> List.sum
            in
            tree
                { label
                    | height = height
                    , connectors = connectors children
                }
                children


colorize : String -> Tree Block -> Tree Block
colorize lastFinalized tree =
    let
        startZipper =
            tree
                |> Zipper.fromTree
                |> Zipper.findFromRoot
                    (.hash >> (==) lastFinalized)

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


nodesAt : List Node -> String -> Int
nodesAt nodes hash =
    nodes
        |> List.filter (\node -> node.bestBlock == hash)
        |> List.length


connectors : List (Tree Block) -> List Int
connectors branches =
    List.map (Tree.label >> .height) branches
        |> List.scanl (+) 0
        |> List.unconsLast
        |> Maybe.withDefault ( 0, [] )
        |> Tuple.second


block : List Node -> Int -> ( Int, Int ) -> List Int -> String -> Block
block nodes height position connectorList hash =
    let
        numNodes =
            List.length nodes

        numNodesAt =
            nodesAt nodes hash
    in
    { hash = hash
    , shortHash = String.left 4 hash
    , numNodesAt = nodesAt nodes hash
    , percentageNodesAt = (numNodesAt |> toFloat) / (numNodes |> toFloat)
    , connectors = connectorList
    , status = Candidate
    , height = height
    }



-- Flatten


type alias FlattenedChain =
    { blocks : List (Positioned Block)
    , numBlocksX : Int
    , numBlocksY : Int
    }


flattenTree : Tree Block -> List (Positioned Block)
flattenTree chain =
    flattenAt (Zipper.fromTree chain) 0 0
        |> Tuple.second


flattenAt : Zipper Block -> Int -> Int -> ( Zipper Block, List (Positioned Block) )
flattenAt layer x y =
    let
        labelAnd : List (Positioned Block) -> List (Positioned Block)
        labelAnd result =
            [ positioned (Zipper.label layer) x y ] ++ result
    in
    case Zipper.nextSibling layer of
        Just sibling ->
            flattenAt sibling x (y + 1)
                |> Tuple.mapSecond labelAnd

        Nothing ->
            case Zipper.firstChild layer of
                Just child ->
                    flattenAt child (x + 1) y
                        |> Tuple.mapSecond labelAnd

                Nothing ->
                    ( layer, labelAnd [] )


positioned : Block -> Int -> Int -> Positioned Block
positioned original x y =
    { hash = original.hash
    , shortHash = original.shortHash
    , numNodesAt = original.numNodesAt
    , percentageNodesAt = original.percentageNodesAt
    , connectors = original.connectors
    , status = original.status
    , height = original.height
    , x = x
    , y = y
    }


unPositioned : Positioned Block -> Block
unPositioned original =
    { hash = original.hash
    , shortHash = original.shortHash
    , numNodesAt = original.numNodesAt
    , percentageNodesAt = original.percentageNodesAt
    , connectors = original.connectors
    , status = original.status
    , height = original.height
    }
