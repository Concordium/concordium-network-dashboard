module RewardGraph exposing
    ( CircularNodeSpec
    , EdgeLabel
    , EdgeSpec
    , NodeSpec(..)
    , RectangularNodeSpec
    , color
    , init
    , nodeCenter
    , tick
    )

import Color exposing (Color)
import Colors
import Graph exposing (Edge, Graph, Node)
import Grid
import IntDict
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Rectangle2d exposing (Rectangle2d)
import Vector2d exposing (Vector2d)


type NodeSpec
    = Circular CircularNodeSpec
    | Rectangular RectangularNodeSpec


type alias CircularNodeSpec =
    { label : List String
    , cx : Float
    , cy : Float
    , radius : Float
    , icon : String
    , color : Color
    , value : Float
    }


type alias RectangularNodeSpec =
    { label : List String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , color : Color
    , value : Float
    }


{-| Describes an Edge in the graph

fromWaypoints is a set of points the edge shoud go through, realtive to the origin node
toWaypoints is a set of points the edge should go through relative to the target node

-}
type alias EdgeSpec =
    { label : EdgeLabel
    , fromWaypoints : List Point2d
    , toWaypoints : List Point2d
    , value : Float
    , animationDelta : Float
    }


type alias EdgeLabel =
    { text : List String
    , position : Float
    , offset : ( Float, Float )
    }


id =
    { blockchain = 0
    , users = 1
    , foundation = 6
    , trustedIdentityIssuers = 3
    , blockBakersBakingPools = 4
    , smartContractDevelopers = 5
    , blockFinalizers = 2
    }


init : Graph NodeSpec EdgeSpec
init =
    let
        rectWidth =
            Grid.offset 14

        rectHeight =
            Grid.offset 6

        rectSpacing =
            Grid.offset 4

        rectSlot i =
            (rectWidth + rectSpacing) * i

        circleRadius =
            Grid.offset 5

        nodes =
            [ Node id.blockchain
                (Circular
                    { label = [ "Blockchain" ]
                    , cx = rectSlot 2 - (rectSpacing / 2)
                    , cy = 400
                    , radius = circleRadius
                    , icon = "/assets/blockchain.svg"
                    , color = Colors.yellow
                    , value = 100
                    }
                )
            , Node id.users
                (Circular
                    { label = [ "Users" ]
                    , cx = rectSlot 1 - (rectSpacing / 2)
                    , cy = 400
                    , radius = circleRadius
                    , icon = "/assets/users.svg"
                    , color = Colors.green
                    , value = 100
                    }
                )
            , Node id.foundation
                (Rectangular
                    { label = [ "Foundation" ]
                    , x = rectSlot 0
                    , y = 600
                    , width = Grid.offset 61
                    , height = rectHeight
                    , color = Colors.blue
                    , value = 100
                    }
                )
            , Node id.trustedIdentityIssuers
                (Rectangular
                    { label = [ "Trusted Identity", "Issuers" ]
                    , x = rectSlot 0
                    , y = 100
                    , width = rectWidth
                    , height = rectHeight
                    , color = Colors.purple
                    , value = 100
                    }
                )
            , Node id.blockBakersBakingPools
                (Rectangular
                    { label = [ "Block Bakers", "Baking Pools" ]
                    , x = rectSlot 1
                    , y = 100
                    , width = rectWidth
                    , height = rectHeight
                    , color = Colors.purple
                    , value = 100
                    }
                )
            , Node id.smartContractDevelopers
                (Rectangular
                    { label = [ "Smart Contract", "Developers" ]
                    , x = rectSlot 2
                    , y = 100
                    , width = rectWidth
                    , height = rectHeight
                    , color = Colors.purple
                    , value = 100
                    }
                )
            , Node id.blockFinalizers
                (Rectangular
                    { label = [ "Block Finalizers" ]
                    , x = rectSlot 3 - rectWidth / 2
                    , y = 400 - (rectHeight / 2)
                    , width = rectWidth
                    , height = rectHeight
                    , color = Colors.purple
                    , value = 100
                    }
                )
            ]

        edges =
            [ Edge
                id.foundation
                id.users
                { label = EdgeLabel [ "Wallet" ] 0.72 ( 10, 0 )
                , value = 0.1
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 15, Grid.point 0 0 ]
                }
            , Edge
                id.foundation
                id.trustedIdentityIssuers
                { label = EdgeLabel [ "Software" ] 0.57 ( 10, 0 )
                , value = 0.4
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                }
            , Edge
                id.foundation
                id.blockBakersBakingPools
                { label = EdgeLabel [ "Software" ] 0.38 ( 10, 0 )
                , value = 0.5
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                }
            , Edge
                id.foundation
                id.smartContractDevelopers
                { label = EdgeLabel [ "Software" ] 0.47 ( 10, 0 )
                , value = 0.5
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                }
            , Edge
                id.foundation
                id.blockFinalizers
                { label = EdgeLabel [ "Software" ] 0.784 ( 10, 0 )
                , value = 0.5
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 15, Grid.point 0 0 ]
                }
            , Edge
                id.trustedIdentityIssuers
                id.users
                { label = EdgeLabel [ "Identities" ] 0.35 ( 10, 0 )
                , value = 0.9
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 2 0, Grid.point 2 10 ]
                , toWaypoints = []
                }
            , Edge
                id.blockBakersBakingPools
                id.users
                { label = EdgeLabel [ "Reward % Kickback" ] 0.45 ( 10, 0 )
                , value = 1.0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point -4 0, Grid.point -4 10 ]
                , toWaypoints = [ Grid.point 0 -2 ]
                }
            , Edge
                id.blockBakersBakingPools
                id.blockchain
                { label = EdgeLabel [ "Baking" ] 0.4 ( 10, 0 )
                , value = 1.1
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 4 0, Grid.point 4 10 ]
                , toWaypoints = [ Grid.point 0 -2 ]
                }
            , Edge
                id.smartContractDevelopers
                id.users
                { label = EdgeLabel [ "Smart Contracts" ] 0.34 ( 0, -10 )
                , value = 0.2
                , animationDelta = 0
                , fromWaypoints =
                    [ Grid.point 0 0
                    , Grid.point 0 -6
                    , Grid.point -27 -6
                    ]
                , toWaypoints = []
                }
            , Edge
                id.users
                id.blockBakersBakingPools
                { label = EdgeLabel [ "Delegate Stake" ] 0.65 ( 10, 0 )
                , value = 0.3
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 2 ]
                , toWaypoints = [ Grid.point -2 12, Grid.point -2 0 ]
                }
            , Edge
                id.users
                id.blockchain
                { label = EdgeLabel [ "Gas" ] 0.4 ( 0, 10 )
                , value = 1.1
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = []
                }
            , Edge
                id.blockchain
                id.trustedIdentityIssuers
                { label = EdgeLabel [ "Transaction Rewards" ] 0.5 ( 0, 10 )
                , value = 0.5
                , animationDelta = 0
                , fromWaypoints =
                    [ Grid.point 0 0
                    , Grid.point 0 -22
                    , Grid.point -27 -22
                    ]
                , toWaypoints = [ Grid.point 0 0 ]
                }
            , Edge
                id.blockchain
                id.blockBakersBakingPools
                { label = EdgeLabel [ "Block Rewards" ] 0.7 ( 10, 0 )
                , value = 0.4
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 2 ]
                , toWaypoints = [ Grid.point 2 12, Grid.point 2 2 ]
                }
            , Edge
                id.blockchain
                id.smartContractDevelopers
                { label = EdgeLabel [ "Execution Rewards" ] 0.6 ( 10, 0 )
                , value = 0.2
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point -2 10, Grid.point -2 0 ]
                }
            , Edge
                id.blockchain
                id.blockFinalizers
                { label = EdgeLabel [ "Finalization", "Rewards" ] 0.45 ( 0, 0 )
                , value = 0.4
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 1 ]
                , toWaypoints = [ Grid.point 0 1 ]
                }
            , Edge
                id.blockchain
                id.foundation
                { label = EdgeLabel [ "Tax" ] 0.5 ( 10, 0 )
                , value = 1.2
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 0, Grid.point 0 15 ]
                , toWaypoints = []
                }
            , Edge id.blockchain
                id.blockchain
                { label = EdgeLabel [ "GTU Minting" ] 0.5 ( -10, 34 )
                , value = 0.8
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 1 -1, Grid.point 7 5 ]
                , toWaypoints = [ Grid.point 4.5 7.5, Grid.point -1.5 1.5 ]
                }
            , Edge
                id.blockFinalizers
                id.blockchain
                { label = EdgeLabel [ "Finalization" ] 0.55 ( 0, -10 )
                , value = 0.5
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 -1 ]
                , toWaypoints = [ Grid.point 0 -1 ]
                }
            ]
    in
    Graph.fromNodesAndEdges nodes edges


nodeCenter : NodeSpec -> Point2d
nodeCenter node =
    case node of
        Circular cnode ->
            Point2d.fromCoordinates ( cnode.cx, cnode.cy )

        Rectangular rnode ->
            Point2d.fromCoordinates
                ( rnode.x + (rnode.width / 2)
                , rnode.y + (rnode.height / 2)
                )


color : NodeSpec -> Color.Color
color node =
    case node of
        Circular cnode ->
            cnode.color

        Rectangular rnode ->
            rnode.color


tick : Graph NodeSpec EdgeSpec -> Graph NodeSpec EdgeSpec
tick graph =
    Graph.mapContexts
        (\context ->
            let
                edgeSum =
                    \eid edge accumulator -> accumulator + edge.value

                incoming =
                    IntDict.foldr edgeSum 0 context.incoming

                outgoing =
                    IntDict.foldr edgeSum 0 context.outgoing
            in
            { context | node = updateNodeValue (incoming - outgoing) context.node }
        )
        graph


updateNodeValue : Float -> Node NodeSpec -> Node NodeSpec
updateNodeValue delta node =
    case node.label of
        Circular spec ->
            Node node.id (Circular { spec | value = spec.value + delta })

        Rectangular spec ->
            Node node.id (Rectangular { spec | value = spec.value + delta })
