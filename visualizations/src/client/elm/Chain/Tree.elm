module Chain.Tree exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Tree exposing (Tree, singleton, tree)
import Tree.Diff exposing (..)
import Tree.Zipper as Zipper exposing (Zipper)
import Tuple


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


build : List (List comparable) -> List (Tree comparable)
build sequences =
    let
        branches =
            sequencesToBranches sequences
    in
    case Dict.isEmpty branches of
        True ->
            []

        False ->
            branches
                |> Dict.map (\k v -> tree k (build v))
                |> Dict.values
