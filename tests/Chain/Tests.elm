module Chain.Tests exposing (..)

import Chain.Build as Build exposing (Block)
import Chain.DictTree as DTree
import Expect exposing (Expectation)
import Set exposing (Set)
import Test exposing (..)
import Tree exposing (Tree, singleton, tree)


suite : Test
suite =
    describe "In the Chain module"
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
        , describe "when building the tree"
            [ test "blocks are positioned correctly (base case)" <|
                \_ ->
                    let
                        treeA =
                            tree ( 1, "a" ) [ tree ( 2, "b" ) [ tree ( 3, "c" ) [] ] ]
                    in
                    Build.annotate [] 0 treeA
                        |> positions
                        |> Expect.equal
                            (Set.fromList
                                [ ( "a", 1, 0 )
                                , ( "b", 2, 0 )
                                , ( "c", 3, 0 )
                                ]
                            )
            , test "blocks are positioned correctly (a litte more comples)" <|
                \_ ->
                    let
                        treeB =
                            tree ( 1, "a0" )
                                [ tree ( 2, "b0" )
                                    [ tree ( 3, "c0" ) [] ]
                                , tree ( 2, "b1" ) []
                                ]
                    in
                    Build.annotate [] 0 treeB
                        |> positions
                        |> Expect.equal
                            (Set.fromList
                                [ ( "a0", 1, 0 )
                                , ( "b0", 2, 0 )
                                , ( "c0", 3, 0 )
                                , ( "b1", 2, 1 )
                                ]
                            )
            , test "blocks are positioned correctly (even more complex)" <|
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
            , test "blocks are positioned and packed correctly" <|
                \_ ->
                    let
                        treeB =
                            tree ( 1, "a0" )
                                [ tree ( 2, "b0" )
                                    [ tree ( 3, "c0" )
                                        [ tree ( 4, "d0" ) [] ]
                                    , tree ( 3, "c1" ) []
                                    ]
                                , tree ( 2, "b2" ) []
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
                                , ( "b2", 2, 1 )
                                ]
                            )
            ]
        ]


positions : Tree Block -> Set ( String, Int, Int )
positions tree =
    tree
        |> Tree.flatten
        |> List.map (\label -> ( label.hash, label.x, label.y ))
        |> Set.fromList
