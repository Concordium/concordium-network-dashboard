module Chain.Api exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (Color)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import List.Extra as List
import RemoteData exposing (WebData)
import Tree exposing (Tree(..), singleton, tree)
import Tree.Zipper as Zipper exposing (Zipper)


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
    , numNodesAt : Int
    , percentageNodesAt : Float
    , connectors : List Int
    , status : BlockStatus
    , height : Int
    }


type alias History comparable =
    { lastFinalized : comparable
    , history : List comparable
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


prepareBlockSequence node =
    node.finalizedBlock
        :: List.reverse node.ancestorsSinceBestBlock



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



-- Building the chain from a set of Nodes


sequencesToBranches : List (List comparable) -> Dict comparable (List (List comparable))
sequencesToBranches maybeSequences =
    case maybeSequences of
        [] ->
            Dict.fromList []

        sequences ->
            sequences
                |> List.filterMap List.uncons
                |> List.map (Tuple.mapSecond List.singleton)
                |> Dict.fromListDedupe (++)



-- a b c d
--     c d e f
-- -> a b c d e f
-- a b c d e f g
--     c d
-- -> a b c d e f g


buildChains : List (List comparable) -> List (Tree comparable)
buildChains sequences =
    let
        branches =
            sequencesToBranches sequences
    in
    case Dict.isEmpty branches of
        True ->
            []

        False ->
            branches
                |> Dict.map (\k v -> tree k (buildChains v))
                |> Dict.values


writeHistory : Maybe (History comparable) -> List (Tree comparable) -> Maybe (History comparable)
writeHistory maybeHistory trees =
    case maybeHistory of
        Nothing ->
            initHistory trees

        Just history ->
            case trees of
                [] ->
                    Just history

                [ single ] ->
                    -- maybe try to stich with previous tree here?
                    Just history

                a :: b :: rest ->
                    extractHistory a b
                        |> Maybe.andThen (stitchHistories history)


initHistory : List (Tree comparable) -> Maybe (History comparable)
initHistory trees =
    case trees of
        [] ->
            Nothing

        [ single ] ->
            Just
                { lastFinalized = Tree.label single
                , history = []
                }

        a :: b :: rest ->
            extractHistory a b


extractHistory : Tree comparable -> Tree comparable -> Maybe (History comparable)
extractHistory a b =
    case ( historyUpwards a (Tree.label b), historyUpwards b (Tree.label a) ) of
        ( Just aUpwards, Nothing ) ->
            Just aUpwards

        ( Nothing, Just bUpwards ) ->
            Just bUpwards

        _ ->
            Nothing


stitchHistories : History comparable -> History comparable -> Maybe (History comparable)
stitchHistories current next =
    if current.lastFinalized == next.lastFinalized then
        Just current

    else
        case next.history of
            [] ->
                Just
                    { lastFinalized = next.lastFinalized
                    , history = current.history
                    }

            headNext :: tailNext ->
                if headNext == current.lastFinalized then
                    Just
                        { lastFinalized = next.lastFinalized
                        , history = current.history ++ next.history
                        }

                else
                    Nothing


historyUpwards : Tree comparable -> comparable -> Maybe (History comparable)
historyUpwards tree label =
    if Tree.label tree == label then
        Just
            { lastFinalized = label
            , history = []
            }

    else
        let
            zipper =
                tree
                    |> Zipper.fromTree
                    |> Zipper.findNext (\current -> current == label)

            construct history fromZipper =
                case Zipper.parent fromZipper of
                    Nothing ->
                        history

                    Just parent ->
                        construct (Zipper.label parent :: history) parent
        in
        zipper
            |> Maybe.map
                (\zipr ->
                    { lastFinalized = label
                    , history = construct [] zipr
                    }
                )


addBranch : Tree comparable -> List comparable -> ( Bool, Tree comparable )
addBranch tree branch =
    case branch of
        [] ->
            ( False, tree )

        first :: remainingBranch ->
            let
                zipper =
                    Zipper.fromTree tree
            in
            List.foldl
                (\child ( parent, zipr, changed ) ->
                    let
                        ( newZiprChanged, newZipr ) =
                            addBlock zipr ( parent, child )
                    in
                    ( child, newZipr, changed || newZiprChanged )
                )
                ( first, zipper, False )
                remainingBranch
                |> (\( lastElement, newZipper, hasChanged ) ->
                        ( hasChanged, Zipper.tree newZipper )
                   )


addBlock : Zipper comparable -> ( comparable, comparable ) -> ( Bool, Zipper comparable )
addBlock zipper ( parent, child ) =
    case Zipper.findFromRoot ((==) parent) zipper of
        Nothing ->
            ( False, zipper )

        Just zparent ->
            case List.any (\e -> Tree.label e == child) (Zipper.children zparent) of
                True ->
                    ( True
                    , zparent
                        |> Zipper.append (singleton child)
                    )

                False ->
                    ( False, zipper )



-- Annotating the chain to prepare for viewing


annotate : List Node -> List String -> Tree String -> Tree Block
annotate nodes history sourceTree =
    annotateChain nodes sourceTree
        |> annotateAndAppendHistory nodes history 5


annotateChain : List Node -> Tree String -> Tree Block
annotateChain nodes sourceTree =
    Tree.restructure
        (annotateBlock nodes)
        annotateChildren
        sourceTree
        |> Tree.mapLabel (\label -> { label | status = Finalized })


annotateAndAppendHistory : List Node -> List String -> Int -> Tree Block -> Tree Block
annotateAndAppendHistory nodes blocks n currentTree =
    blocks
        |> List.reverse
        |> List.take n
        |> List.map (annotateBlock nodes)
        |> List.foldl
            (\b acc -> tree b [ acc ])
            currentTree


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
    { hash = String.left 4 hash
    , numNodesAt = nodesAt nodes hash
    , percentageNodesAt = (numNodesAt |> toFloat) / (numNodes |> toFloat)
    , connectors = connectorList
    , status = Candidate
    , height = height
    }



-- Flatten


flattenTree : Tree Block -> List (Positioned Block)
flattenTree chain =
    flattenAt (Zipper.fromTree chain) 0 0
        |> Tuple.second


flattenAt : Zipper Block -> Int -> Int -> ( Zipper Block, List (Positioned Block) )
flattenAt layer x y =
    let
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
    , numNodesAt = original.numNodesAt
    , percentageNodesAt = original.percentageNodesAt
    , connectors = original.connectors
    , status = original.status
    , height = original.height
    }
