module ChainTest exposing (..)

import Chain.Build as Build exposing (..)
import Chain.DictTree as DTree
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tree exposing (singleton, tree)


suite : Test
suite =
    describe "The Chain.Build module"
        [ describe "Chain.DTree"
            [ test "addAll creates a valud DTree" <|
                \_ ->
                    DTree.init
                        |> DTree.addAll [ [ "a", "b", "c", "d" ], [ "a", "b", "x", "y", "z" ], [ "y", "p" ] ]
                        |> (\t -> DTree.buildForward 10 "a" t [] tree)
                        |> Expect.equal
                            (tree "a"
                                [ tree "b"
                                    [ tree "c" [ singleton "d" ]
                                    , tree "x" [ tree "y" [ singleton "p", singleton "z" ] ]
                                    ]
                                ]
                            )
            , test "building the tree works from different sequences"
                (\_ ->
                    let
                        treeA =
                            DTree.init
                                |> DTree.addAll [ [ "a", "b", "c", "d", "e" ], [ "e", "f", "g" ] ]
                                |> (\dtree ->
                                        DTree.buildForward
                                            10
                                            (DTree.walkBackwardFrom "g" 4 dtree |> Tuple.second)
                                            dtree
                                            []
                                            Tree.tree
                                   )

                        treeB =
                            DTree.init
                                |> DTree.addAll [ [ "a", "b", "c", "d", "e" ], [ "e", "f", "g" ] ]
                                |> (\t ->
                                        DTree.buildForward
                                            10
                                            "a"
                                            t
                                            []
                                            Tree.tree
                                   )
                    in
                    Expect.equal treeA treeB
                )
            ]
        ]
