module Chain.Connector exposing (connector, connector1)

import Arc2d exposing (endPoint)
import Chain.Spec exposing (Spec)
import Color exposing (Color)
import CubicSpline2d exposing (endControlPoint, startControlPoint)
import Element exposing (Element, alignTop, el, explain)
import Geometry.Svg as Svg
import Point2d
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


connector1 : Spec -> Color -> Element msg
connector1 spec color =
    connector spec color [ 0 ]


connector : Spec -> Color -> List Int -> Element msg
connector spec color positions =
    let
        maxCon =
            Maybe.withDefault 0 (List.maximum positions) + 1

        cWidth =
            spec.gutterWidth

        cHeight =
            ((spec.blockHeight + spec.nodeIndicatorHeight) * toFloat maxCon)
                + (spec.gutterHeight * (toFloat maxCon - 1))
    in
    el [ alignTop ]
        (Element.html <|
            svg
                [ width (px cWidth)
                , height (px cHeight)
                , viewBox 0 0 cWidth cHeight
                ]
                (List.map (connectorPath spec color) positions)
        )


connectorPath : Spec -> Color -> Int -> Svg msg
connectorPath spec color toBlock =
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
