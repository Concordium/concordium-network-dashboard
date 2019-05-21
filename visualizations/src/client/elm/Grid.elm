module Grid exposing (circle, offset, point, position, rectangle)

import Circle2d exposing (Circle2d)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)


gridSize =
    15


position : Float -> Float -> ( Float, Float )
position x y =
    ( x * gridSize, y * gridSize )


offset : Float -> Float
offset o =
    o * gridSize


rectangle : Float -> Float -> Float -> Float -> Rectangle2d
rectangle x y w h =
    Rectangle2d.fromExtrema
        { minX = offset x
        , maxX = offset (x + w)
        , minY = offset y
        , maxY = offset (y + h)
        }


circle : Float -> Float -> Float -> Circle2d
circle x y r =
    Circle2d.withRadius (offset r) (point x y)


point : Float -> Float -> Point2d
point x y =
    Point2d.fromCoordinates (position x y)
