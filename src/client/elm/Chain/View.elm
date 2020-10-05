module Chain.View exposing (..)

import Angle
import Chain.Build exposing (..)
import Chain.Flatten exposing (..)
import Chain.Grid as Grid exposing (GridSpec)
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Color.Manipulate exposing (fadeOut)
import Context exposing (..)
import CubicSpline2d
import Geometry.Svg as Svg
import Maybe.Extra as Maybe
import Palette exposing (Palette)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Svg.Attributes
import Svg.Keyed as Keyed
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
            ++ List.map (viewBlock ctx onBlockClick selectedBlock) chain.blocks
        )


viewBlock :
    Context a
    -> Maybe (String -> msg)
    -> Maybe String
    -> DrawableBlock
    -> ( String, Svg msg )
viewBlock ctx clickMsg selectedBlock ({ hash, rect, color, fractionNodesAt } as block) =
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
                |> Maybe.map (\h -> stroke (Paint color))
                |> Maybe.withDefault (stroke PaintNone)

        alpha =
            color |> Color.toRgba |> .alpha
    in
    ( hash
    , g
        (click ++ [ TypedSvg.Attributes.cursor CursorPointer, opacity (Opacity alpha) ])
        [ Svg.rectangle2d
            [ rx (px 4)
            , ry (px 4)
            , fill (Paint <| fadeOut 0.7 color)
            , highlight
            ]
            rect
        , Svg.translateBy translation <| viewText (String.left 4 hash) 16 color
        , viewNodeFractionBar ctx block
        ]
    )


viewNodeFractionBar : Context a -> DrawableBlock -> Svg msg
viewNodeFractionBar ctx block =
    let
        fullWidth =
            64

        height =
            7

        width =
            block.fractionNodesAt * fullWidth

        translation =
            Vector2d.from
                Point2d.origin
                (Rectangle2d.centerPoint block.rect)
                |> Vector2d.plus (Vector2d.pixels (-fullWidth / 2) -29)
    in
    Svg.translateBy translation <|
        Svg.rectangle2d
            [ fill
                (Paint <|
                    Palette.uiToColor ctx.palette.c3
                )
            , Svg.Attributes.style
                "transition: width 300ms ease-out"
            , rx (px 2)
            , ry (px 2)
            ]
        <|
            Rectangle2d.with
                { x1 = Pixels.pixels 0
                , y1 = Pixels.pixels 0
                , x2 = Pixels.pixels width
                , y2 = Pixels.pixels height
                }


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
