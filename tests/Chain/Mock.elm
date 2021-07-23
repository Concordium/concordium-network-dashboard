module Chain.Mock exposing (treesortExamples)

import Chain.Build as Build exposing (Block, Node)
import Chain.DictTree as DictTree
import Html exposing (node)
import Json.Decode as Decode exposing (decodeString)
import Tree exposing (Tree, singleton)


treesortExamples : List (Tree Block)
treesortExamples =
    treesortExamplesJson |> buildMockTrees


{-| Creates an annotated tree from mock json nodes defined in Chain.Mock
-}
buildMockTrees : String -> List (Tree Block)
buildMockTrees jsonString =
    let
        nodeSnapshots =
            jsonString
                |> decodeString (Decode.list (Decode.list Build.decodeNode))
                |> Result.withDefault []
    in
    nodeSnapshots |> List.map buildMockTree


buildMockTree : List Node -> Tree Block
buildMockTree nodes =
    let
        sequences =
            List.map Build.prepareBlockSequence nodes

        tree =
            DictTree.addAll sequences DictTree.init
    in
    DictTree.buildForward 100 ( 1, "a1" ) tree [] Tree.tree
        |> Build.annotate nodes 1


treesortExamplesJson : String
treesortExamplesJson =
    """
    [
        [
            {
                "nodeName": "Node1",
                "nodeId": "1",
                "bestBlock": "d1",
                "bestBlockHeight": 4,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 0,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "d1",
                    "c1",
                    "b1"
                ]
            },
            {
                "nodeName": "Node1b",
                "nodeId": "1b",
                "bestBlock": "d1",
                "bestBlockHeight": 4,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 1,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "d1",
                    "c1",
                    "b1"
                ]
            },
            {
                "nodeName": "Node2",
                "nodeId": "2",
                "bestBlock": "b2",
                "bestBlockHeight": 2,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 1,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "b2"
                ]
            },
            {
                "nodeName": "Node3",
                "nodeId": "3",
                "bestBlock": "c3",
                "bestBlockHeight": 3,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 1,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "c3",
                    "b2"
                ]
            },
            {
                "nodeName": "Node4",
                "nodeId": "4",
                "bestBlock": "d4",
                "bestBlockHeight": 4,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 1,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "d4",
                    "c3",
                    "b2"
                ]
            }
        ],
        [
            {
                "nodeName": "Node1",
                "nodeId": "1",
                "bestBlock": "d1",
                "bestBlockHeight": 4,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 1,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "d1",
                    "c1",
                    "b1"
                ]
            },
            {
                "nodeName": "Node2",
                "nodeId": "2",
                "bestBlock": "b2",
                "bestBlockHeight": 2,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 1,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "b2"
                ]
            },
            {
                "nodeName": "Node3",
                "nodeId": "3",
                "bestBlock": "c3",
                "bestBlockHeight": 3,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 1,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "c3",
                    "b2"
                ]
            },
            {
                "nodeName": "Node4",
                "nodeId": "4",
                "bestBlock": "d4",
                "bestBlockHeight": 4,
                "finalizedBlock": "a1",
                "finalizedBlockHeight": 1,
                "finalizedBlockParent": "p",
                "ancestorsSinceBestBlock": [
                    "d4",
                    "c3",
                    "b2"
                ]
            }
        ]
    ]
    """
