module Chain.Api exposing (..)

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


type alias Animated a =
    { a
        | animation : Animation
    }


type Animation
    = FadeIn Int Int Int
    | FadeOut Int Int Int
    | Move Int Int Int Int
    | Static Int Int


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



-- Mocking


mockInit : Int -> List Node
mockInit n =
    List.map
        (\nodeId ->
            { nodeName = "Node" ++ String.fromInt nodeId
            , nodeId = String.fromInt nodeId
            , bestBlock = "String"
            , bestBlockHeight = 0
            , finalizedBlock = "String"
            , finalizedBlockHeight = 0
            , ancestorsSinceBestBlock = []
            }
        )
        (List.range 1 n)



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
                (children |> List.sortBy (Tree.label >> .numNodesAt))


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
    , width : Int
    , height : Int
    , numCollapsedBlocksHorizontal : Int
    , numCollapsedBlocksVertical : Int
    }


emptyFlatChain : FlattenedChain
emptyFlatChain =
    { blocks = []
    , width = 0
    , height = 0
    , numCollapsedBlocksHorizontal = 0
    , numCollapsedBlocksVertical = 0
    }


flattenTree : Tree Block -> FlattenedChain
flattenTree chain =
    --let
    --    collapsedH =
    --        lastFinalized.height - (Tree.label chain |> .height)
    --in
    flattenAt (Zipper.fromTree chain) 0 0
        |> Tuple.second
        |> (\blocks ->
                { blocks = blocks
                , width =
                    List.map .x blocks
                        |> List.maximum
                        |> Maybe.withDefault 0
                        |> (+) 1
                , height =
                    List.map .y blocks
                        |> List.maximum
                        |> Maybe.withDefault 0
                        |> (+) 1
                , numCollapsedBlocksHorizontal = 0
                , numCollapsedBlocksVertical = 0
                }
           )


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


animated : Positioned Block -> Animated Block
animated original =
    { hash = original.hash
    , shortHash = original.shortHash
    , numNodesAt = original.numNodesAt
    , percentageNodesAt = original.percentageNodesAt
    , connectors = original.connectors
    , status = original.status
    , height = original.height
    , animation = Static original.x original.y
    }



-- Animate


type AnimationStage
    = Init
    | Animate


type alias AnimatedChain =
    { blocks : List (Animated Block)
    , stage : AnimationStage
    , width : Int
    , height : Int
    , numCollapsedBlocksHorizontal : Int
    , numCollapsedBlocksVertical : Int
    }


emptyAnimatedChain =
    { blocks = []
    , stage = Init
    , width = 0
    , height = 0
    , numCollapsedBlocksHorizontal = 0
    , numCollapsedBlocksVertical = 0
    }


deriveAnimations : FlattenedChain -> FlattenedChain -> AnimationStage -> AnimatedChain
deriveAnimations current next stage =
    { blocks = animateBlocks current.blocks next.blocks
    , stage = Init
    , width = next.width
    , height = max current.height next.height
    , numCollapsedBlocksHorizontal = next.numCollapsedBlocksHorizontal
    , numCollapsedBlocksVertical = next.numCollapsedBlocksVertical
    }


animateMove : Animated Block -> Animated Block -> Animated Block
animateMove fromBlock toBlock =
    case ( fromBlock.animation, toBlock.animation ) of
        ( Static fromX fromY, Static toX toY ) ->
            if fromX == toX && fromY == toY then
                toBlock

            else
                { toBlock | animation = Move fromX fromY toX toY }

        _ ->
            toBlock


animateFadeIn : Int -> Animated Block -> Animated Block
animateFadeIn shift ablock =
    case ablock.animation of
        Static x y ->
            { ablock | animation = FadeIn x y shift }

        _ ->
            ablock


animateFadeOut : Int -> Animated Block -> Animated Block
animateFadeOut shift ablock =
    case ablock.animation of
        Static x y ->
            { ablock | animation = FadeOut x y shift }

        _ ->
            ablock


getShift : Animated Block -> Int
getShift ablock =
    case ablock.animation of
        Move fromX fromY toX toY ->
            fromX - toX

        _ ->
            0


animateBlocks : List (Positioned Block) -> List (Positioned Block) -> List (Animated Block)
animateBlocks blocksOut blocksIn =
    let
        blocksOutTuples =
            blocksOut |> List.map (\blockOut -> ( blockOut.hash, ( Just (animated blockOut), Nothing ) ))

        blocksInTuples =
            blocksIn |> List.map (\blockIn -> ( blockIn.hash, ( Nothing, Just (animated blockIn) ) ))

        blockDict =
            (blocksOutTuples ++ blocksInTuples)
                |> Dict.fromListDedupe (\blockOut blockIn -> ( Tuple.first blockOut, Tuple.second blockIn ))

        moves =
            blockDict
                |> Dict.filterMap
                    (\hash tuple ->
                        case tuple of
                            ( Just fromBlock, Just toBlock ) ->
                                Just (animateMove fromBlock toBlock)

                            _ ->
                                Nothing
                    )
                |> Dict.values

        shift =
            moves
                |> List.head
                |> Maybe.map getShift
                |> Maybe.withDefault 0
    in
    blockDict
        |> Dict.filterMap
            (\hash tuple ->
                case tuple of
                    ( Just blockOut, Nothing ) ->
                        Just <| animateFadeOut shift blockOut

                    ( Nothing, Just blockIn ) ->
                        Just <| animateFadeIn shift blockIn

                    ( Just blockOut, Just blockIn ) ->
                        Just <| animateMove blockOut blockIn

                    _ ->
                        Nothing
            )
        |> Dict.values
