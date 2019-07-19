module SvgWidgets exposing (amountIcon, clock)

import Arc2d exposing (Arc2d)
import Color
import Colors exposing (..)
import Element exposing (Element, el, html)
import Geometry.Svg exposing (arc2d)
import Point2d exposing (Point2d)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)


clock : Int -> Float -> Element msg
clock size progress =
    let
        radius =
            toFloat size / 2
    in
    el []
        (html <|
            svg [ width (px <| toFloat size), height (px <| toFloat size) ]
                [ circle
                    [ cx (percent 50)
                    , cy (percent 50)
                    , r (px radius)
                    , fill <| Fill midGray
                    ]
                    []
                , arc2d [ stroke lightGray, strokeWidth (px 6), fill FillNone ]
                    (Arc2d.with
                        { centerPoint = Point2d.fromCoordinates ( radius, radius )
                        , startAngle = degrees -90
                        , sweptAngle = degrees (progress * 360)
                        , radius = radius - 3
                        }
                    )
                ]
        )


amountIcon : Float -> Float -> Color.Color -> Element msg
amountIcon w h color =
    el []
        (html <|
            svg
                [ width (px w)
                , height (px h)
                , viewBox 0 0 30 30
                , fill <| Fill color
                ]
                [ polygon
                    [ points
                        [ ( 0.85, 5.19 )
                        , ( 6.94, 5.19 )
                        , ( 6.94, 24.91 )
                        , ( 0.85, 24.91 )
                        , ( 0.85, 27.13 )
                        , ( 15.24, 27.13 )
                        , ( 15.24, 24.91 )
                        , ( 9.15, 24.91 )
                        , ( 9.15, 5.19 )
                        , ( 15.24, 5.19 )
                        , ( 15.24, 2.98 )
                        , ( 0.85, 2.98 )
                        , ( 0.85, 5.19 )
                        ]
                    ]
                    []
                , rect [ x (px 19.18), y (px 20.9), width (px 9.96), height (px 6.22) ] []
                , rect [ x (px 19.18), y (px 11.72), width (px 9.96), height (px 6.22) ] []
                , rect [ x (px 19.18), y (px 2.98), width (px 9.96), height (px 6.22) ] []
                ]
        )
