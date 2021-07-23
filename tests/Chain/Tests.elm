module Chain.Tests exposing (..)

import Chain.Build as Build exposing (Block)
import Chain.DictTree as DictTree
import Chain.Mock as Mock
import Expect exposing (Expectation)
import Json.Decode as Decode exposing (decodeString)
import Set exposing (Set)
import Test exposing (..)
import Tree exposing (Tree, singleton, tree)


dictTree : Test
dictTree =
    describe "Chain.DictTree"
        [ test "addAll creates a valid DictTree" <|
            \_ ->
                DictTree.init
                    |> DictTree.addAll [ [ "a", "b", "c", "d" ], [ "a", "b", "x", "y", "z" ], [ "y", "p" ] ]
                    |> (\t -> DictTree.buildForward 10 "a" t [] tree)
                    |> Expect.equal
                        (tree "a"
                            [ tree "b"
                                [ tree "c" [ singleton "d" ]
                                , tree "x" [ tree "y" [ singleton "p", singleton "z" ] ]
                                ]
                            ]
                        )
        ]


build : Test
build =
    describe "Chain.Build"
        [ describe "Build.annotate: basic positioning"
            [ test "base case" <|
                \_ ->
                    let
                        example =
                            tree ( 1, "a" ) [ tree ( 2, "b" ) [ tree ( 3, "c" ) [] ] ]
                    in
                    Build.annotate [] 0 example
                        |> positions
                        |> Expect.equal
                            (Set.fromList
                                [ ( "a", 1, 0 )
                                , ( "b", 2, 0 )
                                , ( "c", 3, 0 )
                                ]
                            )
            , test "multiple branches" <|
                \_ ->
                    let
                        example =
                            tree ( 1, "a0" )
                                [ tree ( 2, "b0" )
                                    [ tree ( 3, "c0" ) [] ]
                                , tree ( 2, "b1" ) []
                                ]
                    in
                    Build.annotate [] 0 example
                        |> positions
                        |> Expect.equal
                            (Set.fromList
                                [ ( "a0", 1, 0 )
                                , ( "b0", 2, 0 )
                                , ( "c0", 3, 0 )
                                , ( "b1", 2, 1 )
                                ]
                            )
            , test "even more branches" <|
                \_ ->
                    let
                        treeB =
                            tree ( 1, "a0" )
                                [ tree ( 2, "b0" )
                                    [ tree ( 3, "c0" )
                                        [ tree ( 4, "d0" ) [] ]
                                    , tree ( 3, "c1" ) []
                                    ]
                                , tree ( 2, "b2" ) [ tree ( 3, "c2" ) [] ]
                                ]
                    in
                    Build.annotate [] 0 treeB
                        |> positions
                        |> Expect.equal
                            (Set.fromList
                                [ ( "a0", 1, 0 )
                                , ( "b0", 2, 0 )
                                , ( "c0", 3, 0 )
                                , ( "d0", 4, 0 )
                                , ( "c1", 3, 1 )
                                , ( "b2", 2, 2 )
                                , ( "c2", 3, 2 )
                                ]
                            )
            , test "blocks are positioned and packed if possible" <|
                \_ ->
                    let
                        example =
                            tree ( 1, "a0" )
                                [ tree ( 2, "b0" )
                                    [ tree ( 3, "c0" )
                                        [ tree ( 4, "d0" ) [] ]
                                    , tree ( 3, "c1" ) []
                                    ]
                                , tree ( 2, "b2" ) []
                                ]
                    in
                    Build.annotate [] 0 example
                        |> positions
                        |> Expect.equal
                            (Set.fromList
                                [ ( "a0", 1, 0 )
                                , ( "b0", 2, 0 )
                                , ( "c0", 3, 0 )
                                , ( "d0", 4, 0 )
                                , ( "c1", 3, 1 )
                                , ( "b2", 2, 1 )
                                ]
                            )
            ]
        , describe "Build.annotate: branch weighting"
            [ test "the branch containing the block with the best probability of finalization should appear at the top" <|
                \_ ->
                    let
                        maybeExampleA =
                            Mock.treesortExamples |> List.head
                    in
                    case maybeExampleA of
                        Just exampleA ->
                            exampleA
                                |> positions
                                |> Expect.equal
                                    (Set.fromList
                                        [ ( "a1", 1, 0 )
                                        , ( "b1", 2, 0 )
                                        , ( "c1", 3, 0 )
                                        , ( "d1", 4, 0 )
                                        , ( "b2", 2, 1 )
                                        , ( "c3", 3, 1 )
                                        , ( "d4", 4, 1 )
                                        ]
                                    )

                        Nothing ->
                            Expect.fail "There was something wrong with the example."
            , test "if there is a draw between branches, the branch with the highest cumulative weight should be at the top" <|
                \_ ->
                    let
                        maybeExampleB =
                            Mock.treesortExamples |> List.drop 1 |> List.head
                    in
                    case maybeExampleB of
                        Just exampleB ->
                            exampleB
                                |> positions
                                |> Expect.equal
                                    (Set.fromList
                                        [ ( "a1", 1, 0 )
                                        , ( "b1", 2, 1 )
                                        , ( "c1", 3, 1 )
                                        , ( "d1", 4, 1 )
                                        , ( "b2", 2, 0 )
                                        , ( "c3", 3, 0 )
                                        , ( "d4", 4, 0 )
                                        ]
                                    )

                        Nothing ->
                            Expect.fail "There was something wrong with the example."
            ]
        ]


{-| A helper function for checking correct block positioning.
Creates a set of tuples assiating blockId with x and y positioning.
-}
positions : Tree Block -> Set ( String, Int, Int )
positions tree =
    tree
        |> Tree.map (\label -> ( label.hash, label.x, label.y ))
        |> Tree.flatten
        |> Set.fromList
