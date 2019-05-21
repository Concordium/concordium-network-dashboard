module RewardGraph exposing
    ( CircularNodeSpec
    , EdgeSpec
    , NodeSpec(..)
    , RectangularNodeSpec
    , color
    , init
    , nodeCenter
    )

import Color exposing (Color)
import Colors
import Graph exposing (Edge, Graph, Node)
import Grid
import Point2d exposing (Point2d)
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
    { label : String
    , fromWaypoints : List Point2d
    , toWaypoints : List Point2d
    , value : Float
    , animationDelta : Float
    }


id =
    { blockchain = 0
    , users = 1
    , foundation = 2
    , trustedIdentityIssuers = 3
    , blockBakersBakingPools = 4
    , smartContractDevelopers = 5
    , blockFinalizers = 6
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
            100.0 + ((rectWidth + rectSpacing) * i)

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
                    }
                )
            , Node id.foundation
                (Rectangular
                    { label = [ "Foundation" ]
                    , x = 100
                    , y = 600
                    , width = Grid.offset 61
                    , height = rectHeight
                    , color = Colors.blue
                    , value = 20
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
                    , value = 40
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
                    , value = 45
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
                    , value = 15
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
                    , value = 13
                    }
                )
            ]

        edges =
            [ Edge
                id.trustedIdentityIssuers
                id.users
                { label = "Identities"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 2 0, Grid.point 2 10 ]
                , toWaypoints = []
                }
            , Edge
                id.blockBakersBakingPools
                id.users
                { label = "Reward % Kickback"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point -4 0, Grid.point -4 10 ]
                , toWaypoints = [ Grid.point 0 -2 ]
                }
            , Edge
                id.blockBakersBakingPools
                id.blockchain
                { label = "Baking"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 4 0, Grid.point 4 10 ]
                , toWaypoints = [ Grid.point 0 -2 ]
                }
            , Edge
                id.smartContractDevelopers
                id.users
                { label = "Smart Contracts"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 0, Grid.point 0 -6, Grid.point -27 -6 ]
                , toWaypoints = []
                }
            , Edge
                id.users
                id.blockBakersBakingPools
                { label = "Delegate Stake"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 2 ]
                , toWaypoints = [ Grid.point -2 12, Grid.point -2 0 ]
                }
            , Edge
                id.users
                id.blockchain
                { label = "Gas"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = []
                }
            , Edge
                id.blockchain
                id.trustedIdentityIssuers
                { label = "Transaction Rewards"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 0, Grid.point 0 -22, Grid.point -27 -22 ]
                , toWaypoints = [ Grid.point 0 0 ]
                }
            , Edge
                id.blockchain
                id.blockBakersBakingPools
                { label = "Block Rewards"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 2 ]
                , toWaypoints = [ Grid.point 2 12, Grid.point 2 2 ]
                }
            , Edge
                id.blockchain
                id.smartContractDevelopers
                { label = "Execution Rewards"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point -2 10, Grid.point -2 0 ]
                }
            , Edge
                id.blockchain
                id.blockFinalizers
                { label = "Finalization Rewards"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 1 ]
                , toWaypoints = [ Grid.point 0 1 ]
                }
            , Edge
                id.blockFinalizers
                id.blockchain
                { label = "Finalization"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 -1 ]
                , toWaypoints = [ Grid.point 0 -1 ]
                }
            , Edge
                id.blockchain
                id.foundation
                { label = "Tax"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 0 0, Grid.point 0 15 ]
                , toWaypoints = []
                }
            , Edge
                id.foundation
                id.users
                { label = "Wallet"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 15, Grid.point 0 0 ]
                }
            , Edge
                id.foundation
                id.trustedIdentityIssuers
                { label = "Software"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                }
            , Edge
                id.foundation
                id.blockBakersBakingPools
                { label = "Software"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                }
            , Edge
                id.foundation
                id.smartContractDevelopers
                { label = "Software"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                }
            , Edge
                id.foundation
                id.blockFinalizers
                { label = "Software"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = []
                , toWaypoints = [ Grid.point 0 15, Grid.point 0 0 ]
                }
            , Edge id.blockchain
                id.blockchain
                { label = "GTU Minting"
                , value = 0
                , animationDelta = 0
                , fromWaypoints = [ Grid.point 1 -1, Grid.point 7 5 ]
                , toWaypoints = [ Grid.point 4.5 7.5, Grid.point -1.5 1.5 ]
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
