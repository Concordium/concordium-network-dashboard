module Chain.Tree exposing (..)

import Chain.Api as Api exposing (..)
import Chain.Spec exposing (..)
import List.Extra as List
import Tree exposing (Tree(..), singleton, tree)
import Tree.Zipper as Zipper exposing (Zipper)


growTree : Maybe (Tree comparable) -> List (List comparable) -> Maybe (Tree comparable)
growTree maybeTree newBranches =
    case ( maybeTree, newBranches ) of
        ( Nothing, [] ) ->
            Nothing

        ( Nothing, firstBranch :: otherBranches ) ->
            case firstBranch of
                [] ->
                    growTree Nothing otherBranches

                seqHead :: seqTail ->
                    growTree (Just <| growBranch seqHead seqTail) otherBranches

        ( Just t, _ ) ->
            Just <|
                Zipper.toTree <|
                    List.foldl
                        (\brnch zipr -> alignAndZipBranch zipr brnch)
                        (Zipper.fromTree t)
                        newBranches



-- Grows a single branch from a list (resulting in a single branch tree)


growBranch : comparable -> List comparable -> Tree comparable
growBranch start sequence =
    case sequence of
        [] ->
            singleton start

        seqHead :: seqTail ->
            tree start [ growBranch seqHead seqTail ]


alignAndZipBranch : Zipper comparable -> List comparable -> Zipper comparable
alignAndZipBranch zipper branch =
    case branch of
        [] ->
            zipper

        branchHead :: branchTail ->
            if Zipper.label zipper == branchHead then
                zipBranch zipper branchTail

            else
                case Zipper.findNext ((==) branchHead) zipper of
                    Nothing ->
                        zipper

                    Just atBranchHead ->
                        -- skip existing, continue one level down
                        zipBranch atBranchHead branchTail


zipBranch : Zipper comparable -> List comparable -> Zipper comparable
zipBranch zipper branch =
    case branch of
        [] ->
            zipper

        branchHead :: branchTail ->
            let
                maybeMatchingChild =
                    Zipper.children zipper
                        |> List.filter (\child -> Tree.label child == branchHead)
                        |> List.head
                        |> Maybe.map Zipper.fromTree
            in
            case maybeMatchingChild of
                Nothing ->
                    Zipper.append
                        (growBranch branchHead branchTail)
                        zipper

                Just matchingChild ->
                    -- skip existing, continue one level down
                    zipBranch matchingChild branchTail
