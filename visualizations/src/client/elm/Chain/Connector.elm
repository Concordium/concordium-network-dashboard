module Chain.Connector exposing (connector)

import Arc2d exposing (endPoint)
import Chain.Spec exposing (Spec)
import Color exposing (Color)
import CubicSpline2d exposing (endControlPoint, startControlPoint)
import Element exposing (Element, alignTop, el)
import Geometry.Svg as Svg
import Point2d
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


connector : Spec -> Color -> Int -> Element msg
connector spec color numBlocks =
    let
        cWidth =
            spec.gutterWidth

        cHeight =
            spec.blockHeight * toFloat numBlocks + spec.gutterHeight * (toFloat numBlocks - 1)
    in
    el [ alignTop ]
        (Element.html <|
            svg
                [ width (px cWidth)
                , height (px cHeight)
                , viewBox 0 0 cWidth cHeight
                ]
                (List.map (connectorPath spec color numBlocks) (List.range 0 (numBlocks - 1)))
        )


connectorPath : Spec -> Color -> Int -> Int -> Svg msg
connectorPath spec color numBlocks toBlock =
    let
        spline =
            CubicSpline2d.with
                { startPoint = Point2d.fromCoordinates ( 0, 0.5 * spec.blockHeight )
                , startControlPoint = Point2d.fromCoordinates ( 2 * spec.gutterWidth / 3.0, 0.5 * spec.blockHeight )
                , endControlPoint =
                    Point2d.fromCoordinates
                        ( spec.gutterWidth / 3.0
                        , 0.5
                            * spec.blockHeight
                            + toFloat toBlock
                            * (spec.gutterHeight + spec.blockHeight)
                        )
                , endPoint =
                    Point2d.fromCoordinates
                        ( spec.gutterWidth
                        , 0.5
                            * spec.blockHeight
                            + toFloat toBlock
                            * (spec.gutterHeight + spec.blockHeight)
                        )
                }
    in
    Svg.cubicSpline2d
        [ fill FillNone, stroke color, strokeWidth (px 2) ]
        spline
