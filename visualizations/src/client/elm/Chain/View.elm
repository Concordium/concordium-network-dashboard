module Chain.View exposing (..)

import Chain.Build as Build exposing (..)
import Chain.Flatten as Flatten exposing (..)
import Color exposing (Color, rgb)
import Color.Interpolate exposing (..)
import Colors exposing (fromUI, toUI)
import CubicSpline2d exposing (fromControlPoints)
import Geometry.Svg as Svg
import GeometryUtils exposing (TopLeftCoordinates)
import Grid exposing (GridSpec)
import LineSegment2d exposing (LineSegment2d)
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import Rectangle2d exposing (Rectangle2d)
import Rectangle2d.Extra as Rectangle2d
import Svg.Keyed as Keyed
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)
import Vector2d exposing (Vector2d)


viewChain : GridSpec -> ProtoBlock -> List Node -> DrawableChain -> Svg msg
viewChain gridSpec lastFinalized nodes chain =
    let
        ( viewWidth, viewHeight ) =
            Grid.dimensions gridSpec chain.width chain.height
                |> Tuple.mapBoth Pixels.inPixels Pixels.inPixels
                |> Tuple.mapBoth
                    ((+) (gridSpec.outerPadding * 2))
                    ((+) (gridSpec.outerPadding * 2))
    in
    Keyed.node "svg"
        [ width (px viewWidth)
        , height (px viewHeight)
        , viewBox -gridSpec.outerPadding -gridSpec.outerPadding viewWidth viewHeight
        ]
        (List.map viewConnector chain.connectors
            ++ List.map viewBlock chain.blocks
            ++ List.map viewNode chain.nodes
            ++ [ viewCollapsedBlocksSummary gridSpec lastFinalized chain ]
        )


{-| An overlay displaying the last finalized Block, when it would be out of view
-}
viewCollapsedBlocksSummary : GridSpec -> ProtoBlock -> DrawableChain -> ( String, Svg msg )
viewCollapsedBlocksSummary gridSpec lastFinalized chain =
    (case chain.numCollapsedBlocksX > 0 of
        True ->
            let
                lastFinalizedBlock =
                    { hash = Tuple.second lastFinalized
                    , color = blockColor Finalized
                    , rect = Grid.cell gridSpec 0 0
                    }

                background =
                    Grid.region gridSpec 0 0 1 chain.height

                rightEdge =
                    LineSegment2d.from
                        (Rectangle2d.interpolate background 1 0)
                        (Rectangle2d.interpolate background 1 1)
            in
            g []
                [ Svg.rectangle2d [ fill <| Paint Colors.background ] background
                , Svg.lineSegment2d
                    [ stroke (Paint <| Colors.fadeToBackground 0.75 lastFinalizedBlock.color)
                    , strokeDasharray "4"
                    ]
                    rightEdge
                , Tuple.second (viewBlock lastFinalizedBlock)
                ]

        False ->
            g [] []
    )
        |> (\value -> ( "summaryX", value ))


viewBlock : DrawableBlock -> ( String, Svg msg )
viewBlock { hash, rect, color } =
    let
        nodesAtBarRect =
            Rectangle2d.splitX 0.2 rect

        translation =
            Vector2d.from
                Point2d.origin
                (Rectangle2d.centerPoint rect)
    in
    ( hash
    , g []
        [ Svg.rectangle2d
            [ rx (px 4)
            , ry (px 4)
            , fill (Paint <| Colors.fadeToBackground 0.75 color)
            ]
            rect
        , Svg.translateBy translation <| viewText (String.left 4 hash) color
        ]
    )


viewText : String -> Color -> Svg msg
viewText line color =
    text_
        [ textAnchor AnchorMiddle
        , alignmentBaseline AlignmentCentral
        , fill <| Paint color
        , fontSize (px 16)
        , fontFamily [ "Roboto Mono", "monospaces" ]
        ]
        [ text line ]


viewConnector : DrawableConnector -> ( String, Svg msg )
viewConnector { id, start, end, color } =
    let
        spline =
            CubicSpline2d.fromControlPoints
                start
                (start |> Point2d.translateBy (Vector2d.pixels 30 0))
                (end |> Point2d.translateBy (Vector2d.pixels -30 0))
                end
    in
    ( id
    , Svg.cubicSpline2d
        [ fill PaintNone
        , stroke (Paint <| Colors.fadeToBackground 0.75 color)
        , strokeWidth (px 2)
        ]
        spline
    )


viewNode : DrawableNode -> ( String, Svg msg )
viewNode node =
    ( node.nodeId, Svg.circle2d [ fill (Paint Colors.purple) ] node.circle )
