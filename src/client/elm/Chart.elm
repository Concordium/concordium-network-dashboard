module Chart exposing (main, test)

-- import SampleData exposing (timeSeries)

import Axis
import Color
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


test =
    view timeSeries


timeSeries : List ( Time.Posix, Float )
timeSeries =
    [ ( Time.millisToPosix 1448928000000, 2.5 )
    , ( Time.millisToPosix 1451606400000, 2 )
    , ( Time.millisToPosix 1452211200000, 3.5 )
    , ( Time.millisToPosix 1452816000000, 2 )
    , ( Time.millisToPosix 1453420800000, 3 )
    , ( Time.millisToPosix 1454284800000, 1 )
    , ( Time.millisToPosix 1456790400000, 1.2 )
    ]


w : Float
w =
    400


h : Float
h =
    200


padding : Float
padding =
    0


xScale : ContinuousScale Time.Posix
xScale =
    Scale.time Time.utc ( 0, w - 2 * padding ) ( Time.millisToPosix 1448928000000, Time.millisToPosix 1456790400000 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 5 )


yScaleOffset : Float -> ContinuousScale Float
yScaleOffset offset =
    Scale.linear ( (h - 2 * padding) - offset, 0 - offset ) ( 0, 5 )


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] xScale


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData : Float -> ( Time.Posix, Float ) -> Maybe ( Float, Float )
transformToLineData offsetY ( x, y ) =
    Just ( Scale.convert xScale x, Scale.convert (yScaleOffset offsetY) y )


tranfromToAreaData : ( Time.Posix, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData ( x, y ) =
    Just
        ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert xScale x, Scale.convert yScale y )
        )


line : Float -> List ( Time.Posix, Float ) -> Path
line offsetY model =
    List.map (transformToLineData offsetY) model
        |> Shape.line Shape.monotoneInXCurve


area : List ( Time.Posix, Float ) -> Path
area model =
    List.map tranfromToAreaData model
        |> Shape.area Shape.monotoneInXCurve


view : List ( Time.Posix, Float ) -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            []

        -- [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            []

        -- [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (area model) [ strokeWidth 3, fill <| Fill <| rgbac255 0 173 255 0.05 ]
            , Path.element (line 0 model) [ stroke (rgbac255 0 173 255 1), strokeWidth 6, fill FillNone ]
            , Path.element (line -10 model) [ stroke (rgbac255 0 173 255 0.15), strokeWidth 20, fill FillNone ]
            ]
        ]



-- From here onwards this is simply example boilerplate.
-- In a real app you would load the data from a server and parse it, perhaps in
-- a separate module.


main =
    view timeSeries
