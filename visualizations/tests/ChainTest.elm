module ChainTest exposing (..)

import Chain.Api as Api exposing (..)
import Chain.DTree as DTree
import Chain.Tree as CTree
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tree exposing (singleton, tree)


testTree =
    let
        seqs =
            [ [ "a", "b", "c", "d" ], [ "a", "b", "x", "y", "z" ] ]
    in
    Api.buildChains seqs
        |> List.head
        |> Maybe.withDefault (singleton "This doesn't happen")


suite : Test
suite =
    describe "The Chain.Api module"
        [ describe "Chain.Api.buildChain"
            [ test "builds up the expected tree from two lists" <|
                \_ ->
                    testTree
                        |> Expect.equal
                            (tree "a"
                                [ tree "b"
                                    [ tree "c" [ singleton "d" ]
                                    , tree "x" [ tree "y" [ singleton "z" ] ]
                                    ]
                                ]
                            )
            ]
        , describe "Chain.Api.historyUpwards"
            [ test "constructs the right chain" <|
                \_ ->
                    Api.historyUpwards testTree "d"
                        |> Expect.equal (Just { lastFinalized = "d", history = [ "a", "b", "c" ] })
            , test "returns [] if the key is the root" <|
                \_ ->
                    Api.historyUpwards testTree "a"
                        |> Expect.equal (Just { lastFinalized = "a", history = [] })
            , test "returns Nothing if key is not in tree" <|
                \_ ->
                    Api.historyUpwards testTree "notInTree"
                        |> Expect.equal Nothing
            ]
        , describe "Chain.Tree.growBranch"
            [ test "Grows branch from a list" <|
                \_ ->
                    CTree.growBranch "a" [ "b", "c", "d" ]
                        |> Expect.equal (tree "a" [ tree "b" [ tree "c" [ singleton "d" ] ] ])
            ]
        , describe "Chain.DTree"
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
            ]
        ]
