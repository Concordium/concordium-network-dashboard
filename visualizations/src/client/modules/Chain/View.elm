module Chain.View exposing (..)

import Chain.Build as Build exposing (..)
import Chain.Flatten as Flatten exposing (..)
import Color exposing (Color, rgb)
import Color.Interpolate exposing (..)
import Color.Manipulate exposing (fadeOut)
import Context exposing (..)
import CubicSpline2d exposing (fromControlPoints)
import Element
import Geometry.Svg as Svg
import GeometryUtils exposing (TopLeftCoordinates)
import Grid exposing (GridSpec)
import LineSegment2d exposing (LineSegment2d)
import Maybe.Extra as Maybe
import Palette exposing (Palette)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Quantity
import Rectangle2d exposing (Rectangle2d)
import Rectangle2d.Extra as Rectangle2d
import Svg.Keyed as Keyed
import Svg.Lazy as Lazy
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (..)
import Vector2d exposing (Vector2d)


type alias ViewSettings msg =
    { gridSpec : GridSpec
    , lastFinalized : ProtoBlock
    , nodes : List Node
    , onBlockClick : Maybe (String -> msg)
    , selectedBlock : Maybe String
    }


viewDimensions : GridSpec -> Int -> Int -> ( Float, Float )
viewDimensions gridSpec vwidth vheight =
    Grid.dimensions gridSpec vwidth vheight
        |> Tuple.mapBoth Pixels.inPixels Pixels.inPixels
        |> Tuple.mapBoth
            ((+) (gridSpec.outerPadding * 2))
            ((+) (gridSpec.outerPadding * 2))


viewChain : Context a -> ViewSettings msg -> DrawableChain -> Svg msg
viewChain ctx { gridSpec, lastFinalized, nodes, onBlockClick, selectedBlock } chain =
    let
        ( viewWidth, viewHeight ) =
            viewDimensions gridSpec chain.width chain.height
    in
    Keyed.node "svg"
        [ width (px viewWidth)
        , height (px viewHeight)
        , viewBox
            (chain.viewBoxOffsetX - gridSpec.outerPadding)
            -gridSpec.outerPadding
            viewWidth
            viewHeight
        ]
        (List.map viewConnector chain.connectors
            ++ List.map (viewBlock onBlockClick selectedBlock) chain.blocks
            ++ List.map (viewNode ctx.palette) chain.nodes
        )


{-| An overlay displaying the last finalized Block, when it would be out of view
-}
viewCollapsedBlocksSummary : Context a -> ViewSettings msg -> DrawableChain -> Svg msg
viewCollapsedBlocksSummary ctx { gridSpec, lastFinalized, nodes, onBlockClick, selectedBlock } chain =
    case chain.numCollapsedBlocksX > 0 of
        True ->
            let
                gridSpecZero =
                    { gridSpec | initialOffsetX = 0 }

                ( viewWidth, viewHeight ) =
                    viewDimensions gridSpecZero 1 chain.height

                lastFinalizedBlock =
                    { hash = Tuple.second lastFinalized
                    , color = blockColor ctx.palette Finalized
                    , rect = Grid.cell gridSpecZero 0 0
                    }

                background =
                    Grid.region gridSpecZero -1 0 1 chain.height
                        |> Rectangle2d.inset (pixels 0) (pixels 22)

                rightEdge =
                    LineSegment2d.from
                        (Rectangle2d.interpolate background 1 0.2)
                        (Rectangle2d.interpolate background 1 1)

                translation =
                    Vector2d.from
                        Point2d.origin
                        (Rectangle2d.interpolate background 1 0)

                color =
                    Palette.withAlphaCo 0.3 (blockColor ctx.palette Candidate)
            in
            svg
                [ width (px (viewWidth - gridSpec.outerPadding + (gridSpec.gutterWidth / 2)))
                , height (px viewHeight)
                , viewBox
                    -gridSpec.outerPadding
                    -gridSpec.outerPadding
                    (viewWidth - gridSpec.outerPadding + (gridSpec.gutterWidth / 2))
                    viewHeight
                ]
                [ Svg.rectangle2d [ fill <| Paint (Palette.uiToColor ctx.palette.bg1) ]
                    background
                , Svg.lineSegment2d
                    [ stroke
                        (Paint <|
                            Palette.withAlphaCo 0.3 color
                        )
                    , strokeDasharray "4"
                    ]
                    rightEdge
                , Svg.translateBy translation <|
                    viewText (fromSignedInt chain.numCollapsedBlocksX) 15 color
                , Tuple.second
                    (viewBlock
                        onBlockClick
                        selectedBlock
                        lastFinalizedBlock
                    )
                ]

        False ->
            g [] []


fromSignedInt : Int -> String
fromSignedInt number =
    if number > 0 then
        "+" ++ String.fromInt number

    else
        String.fromInt number


viewBlock :
    Maybe (String -> msg)
    -> Maybe String
    -> DrawableBlock
    -> ( String, Svg msg )
viewBlock clickMsg selectedBlock { hash, rect, color } =
    let
        translation =
            Vector2d.from
                Point2d.origin
                (Rectangle2d.centerPoint rect)
                |> Vector2d.plus (Vector2d.pixels 0 1)

        click =
            clickMsg
                |> Maybe.map (\cmsg -> [ onClick <| cmsg hash ])
                |> Maybe.withDefault []

        highlight =
            selectedBlock
                |> Maybe.filter ((==) hash)
                |> Maybe.map (\h -> stroke (Paint <| color))
                |> Maybe.withDefault (stroke PaintNone)
    in
    ( hash
    , g
        (click ++ [ TypedSvg.Attributes.cursor CursorPointer ])
        [ Svg.rectangle2d
            [ rx (px 4)
            , ry (px 4)
            , fill (Paint <| fadeOut 0.7 color)
            , highlight
            ]
            rect
        , Svg.translateBy translation <| viewText (String.left 4 hash) 16 color
        ]
    )


viewText : String -> Float -> Color -> Svg msg
viewText line size color =
    text_
        [ textAnchor AnchorMiddle
        , dominantBaseline DominantBaselineMiddle
        , fill <| Paint color
        , fontSize (px size)
        , fontFamily [ "IBM Plex Mono, monospaces" ]
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
        , stroke (Paint <| fadeOut 0.7 color)
        , strokeWidth (px 2)
        ]
        spline
    )


viewNode : Palette Element.Color -> DrawableNode -> ( String, Svg msg )
viewNode palette node =
    ( node.nodeId
    , Svg.circle2d
        [ fill (Paint <| Palette.uiToColor palette.c3) ]
        node.circle
    )
