module NetworkGraph exposing (agedRelations, view)

import Color exposing (Color)
import Dict
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeId)
import IntDict
import List exposing (range)
import Murmur3
import Scale exposing (SequentialScale)
import Scale.Color
import TypedSvg exposing (circle, g, line, polygon, svg, title)
import TypedSvg.Attributes exposing (class, fill, points, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Fill(..))


w : Float
w =
    300


h : Float
h =
    300


colorScale : SequentialScale Color
colorScale =
    Scale.sequential Scale.Color.viridisInterpolator ( 200, 700 )


type alias CustomNode =
    { rank : Int, name : String }


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


init : Graph String () -> Graph Entity ()
init seedGraph =
    let
        graph =
            Graph.mapContexts
                (\({ node, incoming, outgoing } as ctx) ->
                    { incoming = incoming
                    , outgoing = outgoing
                    , node =
                        { label =
                            Force.entity node.id
                                (CustomNode
                                    (IntDict.size incoming + IntDict.size outgoing)
                                    node.label
                                )
                        , id = node.id
                        }
                    }
                )
                seedGraph

        links =
            graph
                |> Graph.edges
                |> List.map
                    (\{ from, to } ->
                        { source = from
                        , target = to
                        , distance = 30
                        , strength = Nothing
                        }
                    )

        forces =
            [ Force.customLinks 1 links
            , Force.manyBodyStrength -30 <| List.map .id <| Graph.nodes graph
            , Force.center (w / 2) (h / 2)
            ]
    in
    Graph.nodes graph
        |> List.map .label
        |> Force.computeSimulation (Force.simulation forces)
        |> updateGraphWithList graph


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        retrieveEntity =
            Maybe.withDefault (Force.entity 0 (CustomNode 0 "")) << Maybe.map (.node >> .label)

        source =
            retrieveEntity <| Graph.get edge.from graph

        target =
            retrieveEntity <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 1
        , stroke <| Scale.convert colorScale source.x
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


hexagon ( x, y ) size attrs =
    let
        angle =
            2 * pi / 6

        p =
            range 0 6
                |> List.map toFloat
                |> List.map (\a -> ( x + cos (a * angle) * size, y + sin (a * angle) * size ))
                |> points
    in
    polygon
        (p :: attrs)


nodeSize size node =
    hexagon ( node.x, node.y )
        size
        [ fill <| Fill <| Scale.convert colorScale node.x
        ]
        [ title [] [ text node.value.name ] ]


nodeElement node =
    if node.label.value.rank < 5 then
        nodeSize 4 node.label

    else if node.label.value.rank < 9 then
        nodeSize 7 node.label

    else if modBy 2 node.label.value.rank == 0 then
        g []
            [ nodeSize 9 node.label
            , circle
                [ r 12
                , cx node.label.x
                , cy node.label.y
                , fill FillNone
                , stroke <| Scale.convert colorScale node.label.x
                ]
                []
            ]

    else
        nodeSize 10 node.label


view model =
    svg [ viewBox 0 0 w h ]
        [ g [ class [ "links" ] ] <| List.map (linkElement model) <| Graph.edges model
        , g [ class [ "nodes" ] ] <| List.map nodeElement <| Graph.nodes model
        ]



-- main =
--     init miserablesGraph |> view


agedRelations nodesDict =
    let
        nGraph =
            Graph.fromNodesAndEdges nodesAsNodeList nodesAsEdgeList

        -- |> Graph.insert
        --     { node = Node (nameAsId "abc") "abc"
        --     , incoming =
        --         [ "node-0", "node-1", "node-2" ]
        --             |> List.map (\nodeId_ -> ( nameAsId nodeId_, () ))
        --             |> IntDict.fromList
        --     , outgoing =
        --         [ "node-0", "node-1", "node-2" ]
        --             |> List.map (\nodeId_ -> ( nameAsId nodeId_, () ))
        --             |> IntDict.fromList
        --     }
        -- nodesDict
        --     |> Dict.toList
        --     |> List.foldl createNode []
        --     |> Graph.insert
        --         { node = Node (nameAsId "abc") "abc"
        --         , incoming =
        --             [ "node0", "node1", "node2" ]
        --                 |> List.map (\nodeId_ -> ( nameAsId nodeId_, () ))
        --                 |> IntDict.fromList
        --         , outgoing =
        --             [ "node0", "node1", "node2" ]
        --                 |> List.map (\nodeId_ -> ( nameAsId nodeId_, () ))
        --                 |> IntDict.fromList
        --         }
        nodesAsNodeList =
            nodesDict
                |> Dict.toList
                |> List.map (\( nodeName, nodeData ) -> Node (nameAsId nodeData.nodeId) nodeData.nodeId)

        nodesAsEdgeList =
            nodesDict
                |> Dict.toList
                |> List.map
                    (\( nodeName, nodeData ) ->
                        List.map
                            (\p ->
                                Edge (nameAsId nodeData.nodeId) (nameAsId p) ()
                            )
                            nodeData.peersList
                    )
                |> List.concat

        -- nodeData.peersList = ["abc", "def", "ghi", "asdf", "blah", "derp", "nice"]
        createNode ( nodeId, nodeData ) graph =
            Graph.insert
                { node = Node (nameAsId nodeId) nodeId
                , incoming =
                    nodeData.peersList
                        |> List.map (\nodeId_ -> ( nameAsId nodeId_, () ))
                        |> IntDict.fromList
                , outgoing =
                    nodeData.peersList
                        |> List.map (\nodeId_ -> ( nameAsId nodeId_, () ))
                        |> IntDict.fromList
                }
                graph

        nameAsId name =
            -- @TODO figure out a better way to get small collission-free ints for graph indices
            modBy 10000 <| Murmur3.hashString 1 name

        -- x =
        --     Debug.log "g" (Graph.edges nGraph)
        test =
            nGraph

        -- Graph.fromNodeLabelsAndEdgePairs
        --     [ "node-1", "node-2" ]
        --     [ ( 23, 44 ) ]
        -- Graph.fromNodesAndEdges [ Node 1 "1", Node 2 "2" ] [ Edge 1 2 (), Edge 2 1 () ]
    in
    init test |> view
