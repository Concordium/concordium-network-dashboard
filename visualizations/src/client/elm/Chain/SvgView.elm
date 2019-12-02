module Chain.SvgView exposing (..)

import Chain.Api exposing (..)
import Chain.Spec exposing (spec)
import Color exposing (Color, rgb)
import Color.Interpolate exposing (..)
import Colors exposing (fromUI, toUI)
import CubicSpline2d exposing (endControlPoint, startControlPoint)
import Geometry.Svg as Svg
import Point2d
import Svg.Keyed as Keyed
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


viewFlattenedChain : FlattenedChain -> Svg msg
viewFlattenedChain chain =
    let
        viewWidth =
            toFloat chain.width
                * spec.blockWidth
                + (toFloat chain.width - 1)
                * spec.gutterWidth
                |> Basics.max 0.0

        viewHeight =
            toFloat chain.height
                * (spec.blockHeight + spec.nodeIndicatorHeight)
                + (toFloat chain.height - 1)
                * spec.gutterHeight
                |> Basics.max 0.0
    in
    Keyed.node "svg"
        [ width (px viewWidth)
        , height (px viewHeight)
        , viewBox 0 0 viewWidth viewHeight
        ]
        (List.map viewPositionedBlock chain.blocks)



--    svg
--        ([ width (px 400), centerX, centerY ] ++ List.map (\b -> inFront (viewPositionedBlock b)) blocks)
--        none
-- -- viewPositionedBlock : Positioned Block -> Svg msg


viewPositionedBlock : Positioned Block -> ( String, Svg msg )
viewPositionedBlock block =
    let
        posX =
            toFloat block.x * (spec.blockWidth + spec.gutterWidth)

        posY =
            toFloat block.y * (spec.blockHeight + spec.gutterHeight + spec.nodeIndicatorHeight)
    in
    ( block.hash
    , g
        [ transform [ Translate posX posY ]
        , width (px <| spec.blockWidth + spec.gutterWidth)
        , height (px <| spec.blockHeight + spec.nodeIndicatorHeight)
        , TypedSvg.Attributes.style "transition: all 500ms ease-out"
        , class [ "fadeIn" ]
        ]
        [ animate [ attributeName "opacity", from 0.0, to 1.0, dur (Duration "500ms") ] []
        , rect
            [ y (px spec.nodeIndicatorHeight)
            , width (px spec.blockWidth)
            , height (px spec.blockHeight)
            , rx (px 4)
            , ry (px 4)
            , fill (Fill <| blockBackground block.status)
            ]
            []
        , viewNodesAtBar block.percentageNodesAt
        , svg
            [ y (px spec.nodeIndicatorHeight)
            , width (px spec.blockWidth)
            , height (px spec.blockHeight)
            ]
            [ viewText (String.left 4 block.hash) (blockColor block.status) ]
        , viewConnector (blockBackground block.status) block.connectors
        ]
    )


viewNodesAtBar : Float -> Svg msg
viewNodesAtBar percentageNodesAt =
    if percentageNodesAt > 0.01 then
        rect
            [ width (px <| percentageNodesAt * spec.blockWidth)
            , height (px 6)
            , rx (px 3)
            , ry (px 3)
            , fill (Fill <| Colors.purple)
            , TypedSvg.Attributes.style "transition: all 500ms ease-out"
            ]
            []

    else
        rect [] []


viewText : String -> Color -> Svg msg
viewText line color =
    text_
        [ x (percent 50)
        , y (percent 50)
        , textAnchor AnchorMiddle
        , alignmentBaseline AlignmentCentral
        , fill <| Fill color
        , fontSize (px 16)
        ]
        [ text line ]


viewConnector : Color -> List Int -> Svg msg
viewConnector color positions =
    let
        maxCon =
            Maybe.withDefault 0 (List.maximum positions) + 1

        cWidth =
            spec.gutterWidth

        cHeight =
            ((spec.blockHeight + spec.nodeIndicatorHeight) * toFloat maxCon)
                + (spec.gutterHeight * (toFloat maxCon - 1))
    in
    svg
        [ x (px spec.blockWidth)
        , width (px cWidth)
        , height (px cHeight)
        , viewBox 0 0 cWidth cHeight
        ]
        (List.map (viewConnectorPath color) positions)


viewConnectorPath : Color -> Int -> Svg msg
viewConnectorPath color toBlock =
    let
        spline =
            CubicSpline2d.with
                { startPoint =
                    Point2d.fromCoordinates
                        ( 0
                        , spec.nodeIndicatorHeight
                            + 0.5
                            * spec.blockHeight
                        )
                , startControlPoint =
                    Point2d.fromCoordinates
                        ( 2 * spec.gutterWidth / 3.0
                        , spec.nodeIndicatorHeight
                            + 0.5
                            * spec.blockHeight
                        )
                , endControlPoint =
                    Point2d.fromCoordinates
                        ( spec.gutterWidth / 3.0
                        , spec.nodeIndicatorHeight
                            + 0.5
                            * spec.blockHeight
                            + toFloat toBlock
                            * (spec.gutterHeight + spec.blockHeight + spec.nodeIndicatorHeight)
                        )
                , endPoint =
                    Point2d.fromCoordinates
                        ( spec.gutterWidth
                        , spec.nodeIndicatorHeight
                            + 0.5
                            * spec.blockHeight
                            + toFloat toBlock
                            * (spec.gutterHeight + spec.blockHeight + spec.nodeIndicatorHeight)
                        )
                }
    in
    Svg.cubicSpline2d
        [ fill FillNone, stroke color, strokeWidth (px 2) ]
        spline


blockColor : BlockStatus -> Color
blockColor status =
    case status of
        Finalized ->
            Colors.green

        LastFinalized ->
            Colors.green

        Candidate ->
            Colors.blue


blockBackground : BlockStatus -> Color
blockBackground status =
    let
        bgAlpha =
            case status of
                LastFinalized ->
                    0.5

                _ ->
                    0.75
    in
    interpolate LAB (blockColor status) Colors.blueishBlack bgAlpha
