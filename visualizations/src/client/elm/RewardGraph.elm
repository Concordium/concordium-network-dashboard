module RewardGraph exposing
    ( CircularNodeDisplay
    , EdgeDisplay
    , EdgeSpec
    , NodeDisplay(..)
    , NodeSpec
    , RectangularNodeDisplay
    , init
    , nodeCenter
    , nodeColor
    , nodeColorFromId
    , outgoingConnectedNodes
    , tick
    , updateEdgeInterval
    , updateEdgeValue
    )

import Color exposing (Color)
import Colors
import Graph exposing (Edge, Graph, Node, nodes)
import Grid
import IntDict
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Rectangle2d exposing (Rectangle2d)
import Round
import Vector2d exposing (Vector2d)


type alias NodeSpec =
    { label : List String
    , value : Float
    , valueString : String
    , display : NodeDisplay
    }


type NodeDisplay
    = Circular CircularNodeDisplay
    | Rectangular RectangularNodeDisplay


type alias CircularNodeDisplay =
    { cx : Float
    , cy : Float
    , radius : Float
    , icon : String
    , color : Color
    }


type alias RectangularNodeDisplay =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , color : Color
    }


type alias EdgeSpec =
    { label : List String
    , value : Float
    , valueString : String
    , interval : Int
    , intervalString : String
    , animationDelta : Float
    , display : EdgeDisplay
    }


{-| Contains the information for displaying the static diagram
_fromWaypoints_ is a set of points the edge shoud go through, realtive to the origin node
_toWaypoints_ is a set of points the edge should go through relative to the target node
-}
type alias EdgeDisplay =
    { fromWaypoints : List Point2d
    , toWaypoints : List Point2d
    , labelPosition : Float
    , labelOffset : ( Float, Float )
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
                { label = [ "Blockchain" ]
                , value = 100
                , valueString = ""
                , display =
                    Circular
                        { cx = rectSlot 2 - (rectSpacing / 2)
                        , cy = 400
                        , radius = circleRadius
                        , icon = "/assets/blockchain.svg"
                        , color = Colors.yellow
                        }
                }
            , Node id.users
                { label = [ "Users" ]
                , value = 100
                , valueString = ""
                , display =
                    Circular
                        { cx = rectSlot 1 - (rectSpacing / 2)
                        , cy = 400
                        , radius = circleRadius
                        , icon = "/assets/users.svg"
                        , color = Colors.green
                        }
                }
            , Node id.foundation
                { label = [ "Foundation" ]
                , value = 100
                , valueString = ""
                , display =
                    Rectangular
                        { x = rectSlot 0
                        , y = 600
                        , width = Grid.offset 61
                        , height = rectHeight
                        , color = Colors.blue
                        }
                }
            , Node id.trustedIdentityIssuers
                { label = [ "Trusted Identity", "Issuers" ]
                , value = 100
                , valueString = ""
                , display =
                    Rectangular
                        { x = rectSlot 0
                        , y = 100
                        , width = rectWidth
                        , height = rectHeight
                        , color = Colors.purple
                        }
                }
            , Node id.blockBakersBakingPools
                { label = [ "Block Bakers", "Baking Pools" ]
                , value = 100
                , valueString = ""
                , display =
                    Rectangular
                        { x = rectSlot 1
                        , y = 100
                        , width = rectWidth
                        , height = rectHeight
                        , color = Colors.purple
                        }
                }
            , Node id.smartContractDevelopers
                { label = [ "Smart Contract", "Developers" ]
                , value = 100
                , valueString = ""
                , display =
                    Rectangular
                        { x = rectSlot 2
                        , y = 100
                        , width = rectWidth
                        , height = rectHeight
                        , color = Colors.purple
                        }
                }
            , Node id.blockFinalizers
                { label = [ "Block Finalizers" ]
                , value = 100
                , valueString = ""
                , display =
                    Rectangular
                        { x = rectSlot 3 - rectWidth / 2
                        , y = 400 - (rectHeight / 2)
                        , width = rectWidth
                        , height = rectHeight
                        , color = Colors.purple
                        }
                }
            ]

        edges =
            [ Edge
                id.foundation
                id.users
                { label = [ "Wallet" ]
                , value = 0.1
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = []
                    , toWaypoints = [ Grid.point 0 15, Grid.point 0 0 ]
                    , labelPosition = 0.72
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.foundation
                id.trustedIdentityIssuers
                { label = [ "Software" ]
                , value = 0.4
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = []
                    , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                    , labelPosition = 0.57
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.foundation
                id.blockBakersBakingPools
                { label = [ "Software" ]
                , value = 0.5
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = []
                    , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                    , labelPosition = 0.38
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.foundation
                id.smartContractDevelopers
                { label = [ "Software" ]
                , value = 0.5
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = []
                    , toWaypoints = [ Grid.point 0 35, Grid.point 0 0 ]
                    , labelPosition = 0.47
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.foundation
                id.blockFinalizers
                { label = [ "Software" ]
                , value = 0.5
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = []
                    , toWaypoints = [ Grid.point 0 15, Grid.point 0 0 ]
                    , labelPosition = 0.784
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.trustedIdentityIssuers
                id.users
                { label = [ "Identities" ]
                , value = 0.9
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point 2 0, Grid.point 2 10 ]
                    , toWaypoints = []
                    , labelPosition = 0.35
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.blockBakersBakingPools
                id.users
                { label = [ "Reward % Kickback" ]
                , value = 1.0
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point -4 0, Grid.point -4 10 ]
                    , toWaypoints = [ Grid.point 0 -2 ]
                    , labelPosition = 0.45
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.blockBakersBakingPools
                id.blockchain
                { label = [ "Baking" ]
                , value = 1.1
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point 4 0, Grid.point 4 10 ]
                    , toWaypoints = [ Grid.point 0 -2 ]
                    , labelPosition = 0.4
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.smartContractDevelopers
                id.users
                { label = [ "Smart Contracts" ]
                , value = 0.2
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints =
                        [ Grid.point 0 0
                        , Grid.point 0 -6
                        , Grid.point -27 -6
                        ]
                    , toWaypoints = []
                    , labelPosition = 0.34
                    , labelOffset = ( 0, -10 )
                    }
                }
            , Edge
                id.users
                id.blockBakersBakingPools
                { label = [ "Delegate Stake" ]
                , value = 0.3
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point 0 2 ]
                    , toWaypoints = [ Grid.point -2 12, Grid.point -2 0 ]
                    , labelPosition = 0.65
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.users
                id.blockchain
                { label = [ "Gas" ]
                , value = 1.1
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = []
                    , toWaypoints = []
                    , labelPosition = 0.4
                    , labelOffset = ( 0, 10 )
                    }
                }
            , Edge
                id.blockchain
                id.trustedIdentityIssuers
                { label = [ "Transaction Rewards" ]
                , value = 0.5
                , interval = 10
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints =
                        [ Grid.point 0 0
                        , Grid.point 0 -22
                        , Grid.point -27 -22
                        ]
                    , toWaypoints = [ Grid.point 0 0 ]
                    , labelPosition = 0.5
                    , labelOffset = ( 0, 10 )
                    }
                }
            , Edge
                id.blockchain
                id.blockBakersBakingPools
                { label = [ "Block Rewards" ]
                , value = 0.4
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point 0 2 ]
                    , toWaypoints = [ Grid.point 2 12, Grid.point 2 2 ]
                    , labelPosition = 0.7
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.blockchain
                id.smartContractDevelopers
                { label = [ "Execution Rewards" ]
                , value = 0.2
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = []
                    , toWaypoints = [ Grid.point -2 10, Grid.point -2 0 ]
                    , labelPosition = 0.6
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge
                id.blockchain
                id.blockFinalizers
                { label = [ "Finalization", "Rewards" ]
                , value = 0.4
                , interval = 30
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point 0 1 ]
                    , toWaypoints = [ Grid.point 0 1 ]
                    , labelPosition = 0.45
                    , labelOffset = ( 0, 0 )
                    }
                }
            , Edge
                id.blockchain
                id.foundation
                { label = [ "Tax" ]
                , value = 1.2
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point 0 0, Grid.point 0 15 ]
                    , toWaypoints = []
                    , labelPosition = 0.5
                    , labelOffset = ( 10, 0 )
                    }
                }
            , Edge id.blockchain
                id.blockchain
                { label = [ "GTU Minting" ]
                , value = 1
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point 1 -1, Grid.point 7 5 ]
                    , toWaypoints = [ Grid.point 4.5 7.5, Grid.point -1.5 1.5 ]
                    , labelPosition = 0.5
                    , labelOffset = ( -10, 34 )
                    }
                }
            , Edge
                id.blockFinalizers
                id.blockchain
                { label = [ "Finalization" ]
                , value = 1
                , interval = 20
                , valueString = ""
                , intervalString = ""
                , animationDelta = 0
                , display =
                    { fromWaypoints = [ Grid.point 0 -1 ]
                    , toWaypoints = [ Grid.point 0 -1 ]
                    , labelPosition = 0.55
                    , labelOffset = ( 0, -10 )
                    }
                }
            ]
    in
    Graph.fromNodesAndEdges nodes edges
        |> updateFormStrings


updateFormStrings : Graph NodeSpec EdgeSpec -> Graph NodeSpec EdgeSpec
updateFormStrings graph =
    graph
        |> Graph.mapNodes (\node -> { node | valueString = Round.round 2 node.value })
        |> Graph.mapEdges
            (\edge ->
                { edge
                    | valueString = Round.round 2 edge.value
                    , intervalString = String.fromInt edge.interval
                }
            )


outgoingConnectedNodes : Int -> Graph NodeSpec EdgeSpec -> List NodeSpec
outgoingConnectedNodes nodeId graph =
    Graph.get nodeId graph
        |> Maybe.map Graph.alongOutgoingEdges
        |> Maybe.withDefault []
        |> (\ids -> List.filter (\a -> List.member a.id ids) (Graph.nodes graph))
        |> List.map .label


updateEdgeValue : Int -> Int -> String -> Graph NodeSpec EdgeSpec -> Graph NodeSpec EdgeSpec
updateEdgeValue fromId toId valueString graph =
    let
        maybeNewValue =
            String.toFloat valueString

        nodes =
            Graph.nodes graph

        edges =
            Graph.edges graph

        newEdges =
            List.map
                (\edge ->
                    if edge.from == fromId && edge.to == toId then
                        let
                            label =
                                edge.label

                            value =
                                Maybe.withDefault edge.label.value maybeNewValue
                        in
                        Edge edge.from
                            edge.to
                            { label
                                | valueString = valueString
                                , value = value
                            }

                    else
                        edge
                )
                edges
    in
    Graph.fromNodesAndEdges nodes newEdges


updateEdgeInterval : Int -> Int -> String -> Graph NodeSpec EdgeSpec -> Graph NodeSpec EdgeSpec
updateEdgeInterval fromId toId intervalString graph =
    let
        maybeNewInterval =
            String.toInt intervalString

        nodes =
            Graph.nodes graph

        edges =
            Graph.edges graph

        newEdges =
            List.map
                (\edge ->
                    if edge.from == fromId && edge.to == toId then
                        let
                            label =
                                edge.label

                            interval =
                                Maybe.withDefault edge.label.interval maybeNewInterval
                        in
                        Edge edge.from
                            edge.to
                            { label
                                | intervalString = intervalString
                                , interval = interval
                            }

                    else
                        edge
                )
                edges
    in
    Graph.fromNodesAndEdges nodes newEdges


nodeCenter : NodeDisplay -> Point2d
nodeCenter node =
    case node of
        Circular cnode ->
            Point2d.fromCoordinates ( cnode.cx, cnode.cy )

        Rectangular rnode ->
            Point2d.fromCoordinates
                ( rnode.x + (rnode.width / 2)
                , rnode.y + (rnode.height / 2)
                )


nodeColor : NodeSpec -> Color.Color
nodeColor node =
    case node.display of
        Circular cnode ->
            cnode.color

        Rectangular rnode ->
            rnode.color


nodeColorFromId : Int -> Graph NodeSpec EdgeSpec -> Color.Color
nodeColorFromId nodeId graph =
    Graph.get nodeId graph
        |> Maybe.map (.node >> .label >> nodeColor)
        |> Maybe.withDefault Color.white


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
    let
        label =
            node.label

        newLabel =
            { label | value = label.value + delta }
    in
    { node | label = newLabel }
