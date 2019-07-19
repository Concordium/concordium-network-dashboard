module RewardGraph.Static exposing (view, viewEdges, viewNodes)

import Arc2d exposing (sweptAngle)
import Color exposing (Color)
import Color.Interpolate as Interpolate exposing (interpolate)
import Colors
import Direction2d exposing (Direction2d)
import Element exposing (paddingXY)
import EllipticalArc2d exposing (startAngle)
import Frame2d
import Geometry.Svg as Svg
import Graph exposing (Edge, Graph, Node, nodes)
import Html.Attributes exposing (selected)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import RewardGraph exposing (..)
import Svg exposing (Svg)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Events exposing (onClick, onMouseOut, onMouseOver)
import TypedSvg.Types exposing (..)
import Types exposing (Msg(..), Window)


view : Window -> Maybe Int -> Graph NodeSpec EdgeSpec -> Svg Msg
view window selected graph =
    let
        width_ =
            toFloat (window.width - 320)
    in
    svg
        [ width (px width_)
        , height (px ((width_ * 3) / 4))
        , viewBox 0 0 920 768
        ]
        [ viewEdges selected graph
        , viewNodes selected (nodes graph)
        ]



-- NODES


viewNodes : Maybe Int -> List (Node NodeSpec) -> Svg Msg
viewNodes selected nodes =
    svg [ width (px 1024), height (px 768), viewBox 0 0 1024 768 ]
        (List.map
            (\node ->
                case node.label.display of
                    Rectangular display ->
                        viewRectangularNode
                            node.id
                            selected
                            node.label.label
                            node.label.value
                            display

                    Circular display ->
                        viewCircularNode
                            node.id
                            selected
                            node.label.label
                            node.label.value
                            display
            )
            nodes
        )


viewRectangularNode : Int -> Maybe Int -> List String -> Float -> RectangularNodeDisplay -> Svg Msg
viewRectangularNode current selected label value display =
    let
        padding =
            8

        valueDisplayHeight =
            normalizeLinear 50 150 value
                |> mapLinear 0 (display.height - (padding * 2))
    in
    svg
        [ x (px display.x)
        , y (px display.y)
        , width (px display.width)
        , height (px display.height)
        , onClick <| NodeHovered (Just current)
        , TypedSvg.Attributes.style "cursor: pointer"
        ]
        [ rect
            [ x (px 0)
            , y (px 0)
            , rx (px 6)
            , ry (px 6)
            , width (percent 100)
            , height (percent 100)
            , fill <| Fill (nodeFill current selected display.color Colors.nodeBackground)
            ]
            []
        , rect
            [ x (px padding)
            , y (px (display.height - padding - valueDisplayHeight))
            , width (px 4)
            , height (px valueDisplayHeight)
            , fill <| Fill (interpolate Interpolate.LAB display.color Colors.nodeBackground 0.5)
            ]
            []
        , viewTextLines label display.color 24 0 16
        ]


mapLinear mapmin mapmax normalizedValue =
    mapmin + (normalizedValue * (mapmax - mapmin))


normalizeLinear mapmin mapmax mappedValue =
    (mappedValue - mapmin) / (mapmax - mapmin)


viewCircularNode : Int -> Maybe Int -> List String -> Float -> CircularNodeDisplay -> Svg Msg
viewCircularNode current selected label value display =
    let
        padding =
            8

        valueDisplayHeight =
            normalizeLinear 50 150 value
                |> mapLinear 0 90

        valueArc =
            Arc2d.with
                { centerPoint = Point2d.fromCoordinates ( display.radius, display.radius )
                , radius = display.radius - padding
                , startAngle = degrees 160
                , sweptAngle = degrees valueDisplayHeight
                }
    in
    svg
        [ x (px <| display.cx - display.radius)
        , y (px <| display.cy - display.radius)
        , width (px <| display.radius * 2.0)
        , height (px <| display.radius * 2.0)
        , onClick <| NodeHovered (Just current)
        , TypedSvg.Attributes.style "cursor: pointer"
        ]
        [ circle
            [ cx (percent 50)
            , cy (percent 50)
            , r (px display.radius)
            , fill <| Fill (nodeFill current selected display.color Colors.nodeBackground)
            ]
            []
        , image
            [ x (percent 50)
            , y (percent 50)
            , width (px display.radius)
            , height (px display.radius)
            , xlinkHref display.icon
            , transform [ Translate (-display.radius / 2) (-display.radius / 2 - 10) ]
            ]
            []
        , viewTextLines label display.color 24 30 16
        , Svg.arc2d
            [ stroke (interpolate Interpolate.LAB display.color Colors.nodeBackground 0.5)
            , strokeWidth (px 4)
            , fill <| FillNone
            ]
            valueArc
        ]


nodeFill : Int -> Maybe Int -> Color -> Color -> Color
nodeFill current selected colorA colorB =
    if Maybe.withDefault -1 selected == current then
        interpolate Interpolate.LAB colorA colorB 0.7

    else
        colorB


viewTextLines : List String -> Color -> Float -> Float -> Float -> Svg msg
viewTextLines lines color lineHeight baseOffset textSize =
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
                    , fontSize (px textSize)
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
            RewardGraph.nodeColor fromNode.label

        isSelected =
            Maybe.withDefault -1 selected == fromNode.id

        transferColor =
            if isSelected then
                baseColor

            else
                interpolate Interpolate.LAB baseColor Colors.nodeBackground 0.4

        trackColor =
            if isSelected then
                interpolate Interpolate.LAB baseColor Colors.nodeBackground 0.3

            else
                interpolate Interpolate.LAB baseColor Colors.nodeBackground 0.7

        fromWaypoints =
            List.map
                (Point2d.placeIn <| Frame2d.atPoint <| nodeCenter fromNode.label.display)
                edge.label.display.fromWaypoints
                |> fallbackIfEmpty (nodeCenter fromNode.label.display)

        toWaypoints =
            List.map
                (Point2d.placeIn <| Frame2d.atPoint <| nodeCenter toNode.label.display)
                edge.label.display.toWaypoints
                |> fallbackIfEmpty (nodeCenter toNode.label.display)

        polyline =
            Polyline2d.fromVertices (fromWaypoints ++ toWaypoints)

        lineLength =
            Polyline2d.length polyline

        dashLength =
            edge.label.value * 50.0

        label =
            if isSelected then
                [ viewEdgeLabel edge.label transferColor polyline ]

            else
                []
    in
    [ Svg.polyline2d
        [ stroke trackColor
        , strokeWidth (px 2)
        , fill <| FillNone
        , strokeDasharray "4 2"
        , strokeDashoffset (String.fromFloat <| -24 * edge.label.animationDelta)
        ]
        polyline
    , Svg.polyline2d
        [ stroke transferColor
        , strokeWidth (px 4)
        , fill <| FillNone
        , strokeDasharray
            (String.fromFloat dashLength
                ++ " "
                ++ String.fromFloat (lineLength + dashLength)
            )
        , strokeDashoffset
            (((dashLength + 10)
                + (1 - edge.label.animationDelta)
                * (lineLength + dashLength)
             )
                |> String.fromFloat
            )
        ]
        polyline
    ]
        ++ label


{-| Finds the longest segment of a polyline and places a label next to it
-}
viewEdgeLabel : EdgeSpec -> Color -> Polyline2d -> Svg msg
viewEdgeLabel edge color edgeLine =
    let
        display =
            edge.display

        ( position, direction ) =
            pointAndDirectionAt edgeLine display.labelPosition

        rotation =
            case direction of
                Nothing ->
                    []

                Just dir ->
                    [ Rotate
                        (if Direction2d.xComponent dir == 0 then
                            -90

                         else
                            0
                        )
                        100
                        100
                    ]

        ( offX, offY ) =
            display.labelOffset
    in
    Maybe.withDefault
        (svg [] [])
        (Maybe.map
            (\( posX, posY ) ->
                svg
                    [ x (px <| posX - 100 + offX)
                    , y (px <| posY - 100 + offY)
                    , width (px 200)
                    , height (px 200)
                    ]
                    [ g [ transform rotation ]
                        [ viewTextLines edge.label color 24 0 12 ]
                    ]
            )
            (Maybe.map Point2d.coordinates position)
        )


fallbackIfEmpty : a -> List a -> List a
fallbackIfEmpty fallback list =
    case list of
        [] ->
            [ fallback ]

        _ ->
            list


pointAndDirectionAt : Polyline2d -> Float -> ( Maybe Point2d, Maybe Direction2d )
pointAndDirectionAt polyline position =
    let
        length =
            Polyline2d.length polyline

        toTravel =
            if position == 0 then
                0

            else
                length * position

        step segments distance =
            case segments of
                [] ->
                    ( Nothing, Nothing )

                seg :: rest ->
                    let
                        segLength =
                            LineSegment2d.length seg
                    in
                    if distance > segLength then
                        step rest (distance - segLength)

                    else
                        ( Just (LineSegment2d.interpolate seg (distance / segLength))
                        , LineSegment2d.direction seg
                        )
    in
    step (Polyline2d.segments polyline) toTravel
