module Chain.DictTree exposing
    ( DictTree
    , addAll
    , buildBackward
    , buildForward
    , init
    , walkBackwardFrom
    , walkForwardFrom
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Set exposing (Set)


type alias DictTree a =
    { forward : Dict a (Set a)
    , backward : Dict a a
    }


init : DictTree a
init =
    { forward = Dict.empty
    , backward = Dict.empty
    }


addAll : List (List comparable) -> DictTree comparable -> DictTree comparable
addAll branches dtree =
    List.foldl
        (\branch updatedTree -> addBranch branch updatedTree)
        dtree
        branches


addBranch : List comparable -> DictTree comparable -> DictTree comparable
addBranch branch dtree =
    case branch of
        [] ->
            dtree

        [ a ] ->
            dtree

        a :: b :: rest ->
            addConnection a b dtree
                |> addBranch (b :: rest)


addConnection : comparable -> comparable -> DictTree comparable -> DictTree comparable
addConnection a b dtree =
    { forward =
        Dict.insertDedupe Set.union a (Set.fromList [ b ]) dtree.forward
    , backward =
        Dict.insert b a dtree.backward
    }


buildForward :
    Int
    -> comparable
    -> DictTree comparable
    -> List comparable
    -> (comparable -> List tree -> tree)
    -> tree
buildForward depth root tree visited construct =
    let
        children =
            Dict.get root tree.forward
                |> Maybe.withDefault Set.empty
                |> Set.toList

        isCycle =
            List.any ((==) root) visited
    in
    if isCycle || depth <= 0 then
        construct root []

    else
        construct root
            (List.map
                (\child ->
                    buildForward
                        (depth - 1)
                        child
                        tree
                        (root :: visited)
                        construct
                )
                children
            )


backward : Int -> Int -> comparable -> DictTree comparable -> ( Int, comparable )
backward depth maxDepth current tree =
    case Dict.get current tree.backward of
        Nothing ->
            ( depth, current )

        Just next ->
            if depth >= maxDepth then
                ( depth, next )

            else
                backward
                    (depth + 1)
                    maxDepth
                    next
                    tree


walkBackwardFrom : comparable -> Int -> DictTree comparable -> ( Int, comparable )
walkBackwardFrom current maxDepth tree =
    backward 0 (max 0 maxDepth) current tree


forward : Int -> Int -> comparable -> DictTree comparable -> List ( Int, comparable )
forward depth maxDepth current tree =
    case Dict.get current tree.forward of
        Nothing ->
            List.singleton ( depth, current )

        Just children ->
            if depth >= maxDepth then
                children
                    |> Set.toList
                    |> List.map (Tuple.pair depth)

            else
                children
                    |> Set.toList
                    |> List.concatMap
                        (\child ->
                            forward
                                (depth + 1)
                                maxDepth
                                child
                                tree
                        )
                    |> List.filter
                        (Tuple.first >> (<) depth)


walkForwardFrom : comparable -> Int -> DictTree comparable -> List ( Int, comparable )
walkForwardFrom current maxDepth tree =
    forward 0 (max 0 maxDepth) current tree


buildBackward :
    Int
    -> comparable
    -> DictTree comparable
    -> (comparable -> List tree -> tree)
    -> tree
buildBackward maxDepth root tree construct =
    let
        buildList : Int -> comparable -> List comparable -> List comparable
        buildList depth current visited =
            case Dict.get current tree.backward of
                Nothing ->
                    visited

                Just previous ->
                    if List.any ((==) previous) visited || depth >= maxDepth then
                        visited

                    else
                        buildList
                            (depth + 1)
                            previous
                            (previous :: visited)
    in
    List.foldr
        (\c acc -> construct c [ acc ])
        (construct root [])
        (buildList 0 root [])
