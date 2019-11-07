module Chain.DTree exposing (DTree, addAll, build, init)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Set exposing (Set)


type Tree a
    = Tree a (List a)


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


build :
    comparable
    -> Dict comparable (List comparable)
    -> List comparable
    -> (comparable -> List tree -> tree)
    -> tree
build root connections visited construct =
    let
        children =
            Dict.get root connections
                |> Maybe.withDefault []

        isCycle =
            List.any ((==) root) visited
    in
    if isCycle then
        construct root []

    else
        construct root
            (List.map
                (\child ->
                    build
                        child
                        connections
                        (root :: visited)
                        construct
                )
                children
            )
