module ChainTest exposing (..)

import Chain.Tree as Tree exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tree exposing (singleton, tree)


suite : Test
suite =
    describe "The Chain.Tree module"
        [ describe "Chain.Tree.build"
            [ test "builds up the expected tree from two lists"
                (\_ ->
                    let
                        chains =
                            [ [ "a", "b", "c", "d" ], [ "a", "b", "x", "y", "z" ] ]

                        result =
                            Debug.log "Tree: \n" (Tree.build chains)
                    in
                    Expect.equal
                        result
                        [ tree "a"
                            [ tree "b"
                                [ tree "c" [ singleton "d" ]
                                , tree "x" [ tree "y" [ singleton "z" ] ]
                                ]
                            ]
                        ]
                )
            ]
        ]
