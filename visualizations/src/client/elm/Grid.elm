module Grid exposing (circle, offset, point, position, rectangle)

import Circle2d exposing (Circle2d)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)


gridSize =
    15


position : Int -> Int -> ( Float, Float )
position x y =
    ( toFloat (x * gridSize), toFloat (y * gridSize) )


offset : Int -> Float
offset o =
    toFloat (o * gridSize)


rectangle : Int -> Int -> Int -> Int -> Rectangle2d
rectangle x y w h =
    Rectangle2d.fromExtrema
        { minX = offset x
        , maxX = offset (x + w)
        , minY = offset y
        , maxY = offset (y + h)
        }


circle : Int -> Int -> Int -> Circle2d
circle x y r =
    Circle2d.withRadius (offset r) (point x y)


point : Int -> Int -> Point2d
point x y =
    Point2d.fromCoordinates (position x y)
