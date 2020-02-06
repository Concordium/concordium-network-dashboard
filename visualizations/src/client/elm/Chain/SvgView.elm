module Chain.SvgView exposing (..)

import Chain.Api exposing (..)
import Chain.Spec exposing (spec)
import Color exposing (Color, rgb)
import Color.Interpolate exposing (..)
import Colors exposing (fromUI, toUI)
import CubicSpline2d exposing (fromControlPoints)
import Geometry.Svg as Svg
import Maybe.Extra as Maybe
import Point2d
import Svg.Keyed as Keyed
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


viewAnimatedChain : Maybe ProtoBlock -> List Node -> AnimatedChain -> Svg msg
viewAnimatedChain maybeLastFinalized nodes chain =
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
                + spec.nodeIndicatorHeight
                |> Basics.max 0.0

        collapsedBlocksSummary =
            maybeLastFinalized
                |> Maybe.unwrap []
                    (\lastFinalized ->
                        case chain.numCollapsedBlocksHorizontal > 0 of
                            True ->
                                [ ( Tuple.second lastFinalized
                                  , viewCollapsedBlocksSummaryHorizontal lastFinalized nodes viewHeight
                                  )
                                ]

                            False ->
                                []
                    )
    in
    case maybeLastFinalized of
        Just lastFinalized ->
            Keyed.node "svg"
                [ width (px viewWidth)
                , height (px viewHeight)
                , viewBox 0 0 viewWidth viewHeight
                ]
                (List.map viewAnimatedBlock chain.blocks
                    ++ collapsedBlocksSummary
                )

        _ ->
            svg [] []



--    svg
--        ([ width (px 400), centerX, centerY ] ++ List.map (\b -> inFront (viewPositionedBlock b)) blocks)
--        none
-- -- viewPositionedBlock : Positioned Block -> Svg msg


viewAnimatedBlock : Animated Block -> ( String, Svg msg )
viewAnimatedBlock block =
    let
        toCoordinates ( x, y ) =
            ( toFloat x * (spec.blockWidth + spec.gutterWidth)
            , toFloat y * (spec.blockHeight + spec.gutterHeight + spec.nodeIndicatorHeight)
            )

        ( fromX, fromY ) =
            (case block.animation of
                Static x y ->
                    ( x, y )

                Move x y _ _ ->
                    ( x, y )

                FadeIn x y shift ->
                    ( x + shift, y )

                FadeOut x y shift ->
                    ( x, y )
            )
                |> toCoordinates

        ( toX, toY ) =
            (case block.animation of
                Static x y ->
                    ( x, y )

                Move _ _ x y ->
                    ( x, y )

                FadeIn x y shift ->
                    ( x, y )

                FadeOut x y shift ->
                    ( x - shift, y )
            )
                |> toCoordinates
    in
    ( block.hash
    , g
        [ transform [ Translate toX toY ]
        , width (px <| spec.blockWidth + spec.gutterWidth)
        , height (px <| spec.blockHeight + spec.nodeIndicatorHeight)
        , class [ "fadeIn" ]
        ]
        [ rect
            [ y (px spec.nodeIndicatorHeight)
            , width (px spec.blockWidth)
            , height (px spec.blockHeight)
            , rx (px 4)
            , ry (px 4)
            , fill (Paint <| blockBackground block.status)
            , TypedSvg.Attributes.style "transition: all 500ms ease-out"
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
            , fill (Paint <| Colors.purple)
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
        , fill <| Paint color
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
            CubicSpline2d.fromControlPoints
                (Point2d.pixels
                    0
                    (spec.nodeIndicatorHeight
                        + 0.5
                        * spec.blockHeight
                    )
                )
                (Point2d.pixels
                    (2 * spec.gutterWidth / 3.0)
                    (spec.nodeIndicatorHeight
                        + 0.5
                        * spec.blockHeight
                    )
                )
                (Point2d.pixels
                    (spec.gutterWidth / 3.0)
                    (spec.nodeIndicatorHeight
                        + 0.5
                        * spec.blockHeight
                        + toFloat toBlock
                        * (spec.gutterHeight + spec.blockHeight + spec.nodeIndicatorHeight)
                    )
                )
                (Point2d.pixels
                    spec.gutterWidth
                    (spec.nodeIndicatorHeight
                        + 0.5
                        * spec.blockHeight
                        + toFloat toBlock
                        * (spec.gutterHeight + spec.blockHeight + spec.nodeIndicatorHeight)
                    )
                )
    in
    Svg.cubicSpline2d
        [ fill PaintNone
        , stroke (Paint color)
        , strokeWidth (px 2)
        , class [ "fadeIn" ]
        ]
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



{--
An overlay that is shown on the left, displaying 
the last finalized Block when it normally would be out
of view
--}


viewCollapsedBlocksSummaryHorizontal : ProtoBlock -> List Node -> Float -> Svg msg
viewCollapsedBlocksSummaryHorizontal lastFinalized nodes viewHeight =
    let
        numNodes =
            nodesAt nodes (Tuple.second lastFinalized)

        lastFinalizedBlock =
            positioned (annotateBlock nodes lastFinalized) 0 0
                |> animated

        w =
            spec.blockWidth + (spec.gutterWidth / 2)

        h =
            spec.blockHeight
    in
    g
        []
        [ rect
            [ width (px <| w + 5)
            , height (percent 100)
            , fill <| Paint Colors.background
            ]
            []
        , line
            [ x1 (px <| w - 1)
            , x2 (px <| w - 1)
            , y1 (percent 0)
            , y2 (percent 100)
            , stroke (Paint <| blockBackground Finalized)
            , strokeDasharray "2"
            ]
            []
        , viewAnimatedBlock { lastFinalizedBlock | status = Finalized }
            |> Tuple.second
        ]
