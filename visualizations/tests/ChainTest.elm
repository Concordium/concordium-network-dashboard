module ChainTest exposing (..)

import Chain.DictTree as DTree
import Expect exposing (Expectation)
import Test exposing (..)
import Tree exposing (singleton, tree)


suite : Test
suite =
    describe "The Chain.Build module"
        [ describe "Chain.DTree"
            [ test "addAll creates a valid DTree" <|
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
            ]
        ]
