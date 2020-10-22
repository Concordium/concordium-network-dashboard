module Chain.Grid exposing (..)

import Angle
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)


type alias GridSpec =
    { cellWidth : Float
    , cellHeight : Float
    , gutterWidth : Float
    , gutterHeight : Float
    , outerPadding : Float
    , initialOffsetX : Int
    }


cell : GridSpec -> Int -> Int -> Rectangle2d Pixels coords
cell spec x y =
    cellRegion spec x y
        |> inset (pixels spec.gutterWidth) (pixels spec.gutterHeight)


cellRegion : GridSpec -> Int -> Int -> Rectangle2d Pixels coords
cellRegion spec x y =
    region spec x y (x + 1) (y + 1)


intersection : GridSpec -> Int -> Int -> Point2d Pixels coords
intersection { cellWidth, cellHeight, gutterWidth, gutterHeight, initialOffsetX } x y =
    Point2d.pixels
        (toFloat (x - initialOffsetX) * (cellWidth + gutterWidth))
        (toFloat y * (cellHeight + gutterHeight))


region : GridSpec -> Int -> Int -> Int -> Int -> Rectangle2d Pixels coords
region spec x1 y1 x2 y2 =
    Rectangle2d.from
        (intersection spec x1 y1)
        (intersection spec x2 y2)


dimensions : GridSpec -> Int -> Int -> ( Quantity Float Pixels, Quantity Float Pixels )
dimensions spec cellsX cellsY =
    region spec 0 0 cellsX cellsY
        |> Rectangle2d.dimensions


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


maxCells : Int -> GridSpec -> Int
maxCells windowWidth { outerPadding, cellWidth, gutterWidth } =
    (toFloat windowWidth - (2 * outerPadding)) / (cellWidth + gutterWidth) |> round


maxCellsWidth : Int -> GridSpec -> Int
maxCellsWidth windowWidth ({ cellWidth, gutterWidth } as spec) =
    let
        ncells =
            maxCells windowWidth spec
    in
    ncells * round cellWidth + (ncells - 1) * round gutterWidth
