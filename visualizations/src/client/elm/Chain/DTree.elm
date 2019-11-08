module Chain.DTree exposing (DTree, addAll, buildBackward, buildForward, init, walkBackward)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Set exposing (Set)


type alias DTree a =
    { forward : Dict a (Set a)
    , backward : Dict a a
    }


init : DTree a
init =
    { forward = Dict.empty
    , backward = Dict.empty
    }


addAll : List (List comparable) -> DTree comparable -> DTree comparable
addAll branches dtree =
    List.foldl
        (\branch updatedTree -> addBranch branch updatedTree)
        dtree
        branches


addBranch : List comparable -> DTree comparable -> DTree comparable
addBranch branch dtree =
    case branch of
        [] ->
            dtree

        [ a ] ->
            dtree

        a :: b :: rest ->
            addConnection a b dtree
                |> addBranch (b :: rest)


addConnection : comparable -> comparable -> DTree comparable -> DTree comparable
addConnection a b dtree =
    { forward = Dict.insertDedupe Set.union a (Set.fromList [ b ]) dtree.forward
    , backward = Dict.insert b a dtree.backward
    }


buildForward :
    Int
    -> comparable
    -> DTree comparable
    -> List comparable
    -> (comparable -> List tree -> tree)
    -> tree
buildForward depth root tree visited construct =
    let
        children =
            Dict.get root tree.forward
                |> Maybe.withDefault Set.empty

        isCycle =
            List.any ((==) root) visited
    in
    if isCycle then
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
                (Set.toList children)
            )


walkBackward : Int -> comparable -> DTree comparable -> comparable
walkBackward depth current tree =
    case Dict.get current tree.backward of
        Nothing ->
            current

        Just previous ->
            if depth <= 0 then
                current

            else
                walkBackward
                    (depth - 1)
                    previous
                    tree


buildBackward :
    Int
    -> comparable
    -> DTree comparable
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
