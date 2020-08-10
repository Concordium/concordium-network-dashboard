module Rectangle2d.Extra exposing (..)

import Angle
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)


inset :
    Quantity Float units
    -> Quantity Float units
    -> Rectangle2d units coordinates
    -> Rectangle2d units coordinates
inset x y rect =
    let
        ( width, height ) =
            Rectangle2d.dimensions rect
                |> Tuple.mapBoth
                    (\w -> w |> Quantity.minus x)
                    (\h -> h |> Quantity.minus y)
    in
    Rectangle2d.withDimensions
        ( width, height )
        (Angle.degrees 0)
        (Rectangle2d.centerPoint rect)


splitX :
    Float
    -> Rectangle2d units coordinates
    -> ( Rectangle2d units coordinates, Rectangle2d units coordinates )
splitX x rect =
    ( Rectangle2d.from
        (Rectangle2d.interpolate rect 0 0)
        (Rectangle2d.interpolate rect x 1)
    , Rectangle2d.from
        (Rectangle2d.interpolate rect x 0)
        (Rectangle2d.interpolate rect 1 1)
    )


splitY :
    Float
    -> Rectangle2d units coordinates
    -> ( Rectangle2d units coordinates, Rectangle2d units coordinates )
splitY y rect =
    ( Rectangle2d.from
        (Rectangle2d.interpolate rect 0 0)
        (Rectangle2d.interpolate rect 1 y)
    , Rectangle2d.from
        (Rectangle2d.interpolate rect 0 y)
        (Rectangle2d.interpolate rect 1 1)
    )
