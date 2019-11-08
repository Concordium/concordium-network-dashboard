module ChainTest exposing (..)

import Chain.Api as Api exposing (..)
import Chain.DTree as DTree
import Chain.Tree as CTree
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tree exposing (singleton, tree)


suite : Test
suite =
    describe "The Chain.Api module"
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
                                |> (\t -> DTree.buildForward 10 (DTree.walkBackward 4 "g" t) t [] Tree.tree)

                        treeB =
                            DTree.init
                                |> DTree.addAll [ [ "a", "b", "c", "d", "e" ], [ "e", "f", "g" ] ]
                                |> (\t -> DTree.buildForward 10 (DTree.walkBackward 4 "g" t) t [] Tree.tree)
                    in
                    Expect.equal treeA treeB
                )
            ]
        ]
