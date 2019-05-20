module ViewRewardGraph exposing (view, viewEdges, viewNodes)

import Color exposing (Color)
import Color.Interpolate as Interpolate exposing (interpolate)
import Colors
import Frame2d
import Geometry.Svg as Svg
import Graph exposing (Edge, Graph, Node, nodes)
import Html.Attributes exposing (selected)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import RewardGraph exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Events exposing (onClick, onMouseOut, onMouseOver)
import TypedSvg.Types exposing (..)
import Types exposing (Msg(..))


view : Maybe Int -> Graph NodeSpec EdgeSpec -> Svg Msg
view selected graph =
    svg [ width (px 1024), height (px 768), viewBox 0 0 1024 768 ]
        [ viewEdges selected graph
        , viewNodes selected (nodes graph)
        ]



-- NODES


viewNodes : Maybe Int -> List (Node NodeSpec) -> Svg Msg
viewNodes selected nodes =
    svg [ width (px 1024), height (px 768), viewBox 0 0 1024 768 ]
        (List.map
            (\node ->
                case node.label of
                    Rectangular props ->
                        viewRectangularNode node.id selected props

                    Circular props ->
                        viewCircularNode node.id selected props
            )
            nodes
        )


viewRectangularNode : Int -> Maybe Int -> RectangularNodeSpec -> Svg Msg
viewRectangularNode current selected props =
    svg
        [ x (px props.x)
        , y (px props.y)
        , width (px props.width)
        , height (px props.height)
        , onMouseOver <| NodeHovered (Just current)
        , onMouseOut <| NodeHovered Nothing
        ]
        [ rect
            [ x (px 0)
            , y (px 0)
            , rx (px 6)
            , ry (px 6)
            , width (percent 100)
            , height (percent 100)
            , fill <| Fill (nodeFill current selected props.color Colors.nodeBackground)
            ]
            []
        , rect
            [ x (px 8)
            , y (px (props.height - 8 - props.value))
            , width (px 4)
            , height (px props.value)
            , fill <| Fill (interpolate Interpolate.LAB props.color Colors.nodeBackground 0.5)
            ]
            []
        , viewTextLines props.label props.color 24 0
        ]


viewCircularNode : Int -> Maybe Int -> CircularNodeSpec -> Svg Msg
viewCircularNode current selected props =
    svg
        [ x (px <| props.cx - props.radius)
        , y (px <| props.cy - props.radius)
        , width (px <| props.radius * 2.0)
        , height (px <| props.radius * 2.0)
        , onMouseOver <| NodeHovered (Just current)
        , onMouseOut <| NodeHovered Nothing
        ]
        [ circle
            [ cx (percent 50)
            , cy (percent 50)
            , r (px props.radius)
            , fill <| Fill (nodeFill current selected props.color Colors.nodeBackground)
            ]
            []
        , image
            [ x (percent 50)
            , y (percent 50)
            , width (px props.radius)
            , height (px props.radius)
            , xlinkHref props.icon
            , transform [ Translate (-props.radius / 2) (-props.radius / 2 - 10) ]
            ]
            []
        , viewTextLines props.label props.color 24 30
        ]


nodeFill : Int -> Maybe Int -> Color -> Color -> Color
nodeFill current selected colorA colorB =
    if Maybe.withDefault -1 selected == current then
        interpolate Interpolate.LAB colorA colorB 0.7

    else
        colorB


viewTextLines : List String -> Color -> Float -> Float -> Svg msg
viewTextLines lines color lineHeight baseOffset =
    let
        numLines =
            toFloat (List.length lines)

        totalHeight =
            numLines * lineHeight

        offset =
            baseOffset + (lineHeight * (0.5 + numLines / 2.0) - totalHeight)
    in
    svg
        []
        (List.indexedMap
            (\index line ->
                text_
                    [ x (percent 50)
                    , y (percent 50)
                    , dy (px (offset + toFloat index * lineHeight))
                    , textAnchor AnchorMiddle
                    , alignmentBaseline AlignmentCentral
                    , fill <| Fill color
                    , fontSize (px 16)
                    ]
                    [ Svg.text line ]
            )
            lines
        )



-- EDGES


viewEdges : Maybe Int -> Graph NodeSpec EdgeSpec -> Svg Msg
viewEdges selected graph =
    svg [ width (px 1024), height (px 768), viewBox 0 0 1024 768 ]
        (List.map
            (\edge ->
                let
                    from =
                        Graph.get edge.from graph

                    to =
                        Graph.get edge.to graph
                in
                case ( from, to ) of
                    ( Just fromContext, Just toContext ) ->
                        viewEdge selected edge fromContext.node toContext.node

                    ( _, _ ) ->
                        []
            )
            (Graph.edges graph)
            |> List.foldr (++) []
        )


viewEdge : Maybe Int -> Edge EdgeSpec -> Node NodeSpec -> Node NodeSpec -> List (Svg Msg)
viewEdge selected edge fromNode toNode =
    let
        baseColor =
            RewardGraph.color fromNode.label

        edgeColor =
            if Maybe.withDefault -1 selected == fromNode.id then
                baseColor

            else
                interpolate Interpolate.LAB baseColor Colors.nodeBackground 0.5

        fromWaypoints =
            List.map
                (Point2d.placeIn <| Frame2d.atPoint <| nodeCenter fromNode.label)
                edge.label.fromWaypoints
                |> setIfEmpty (nodeCenter fromNode.label)

        toWaypoints =
            List.map
                (Point2d.placeIn <| Frame2d.atPoint <| nodeCenter toNode.label)
                edge.label.toWaypoints
                |> setIfEmpty (nodeCenter toNode.label)
    in
    [ Svg.polyline2d
        [ stroke edgeColor
        , strokeWidth (px 2)
        , fill <| FillNone
        ]
        (Polyline2d.fromVertices (fromWaypoints ++ toWaypoints))
    ]


setIfEmpty : a -> List a -> List a
setIfEmpty fallback list =
    case list of
        [] ->
            [ fallback ]

        _ ->
            list
