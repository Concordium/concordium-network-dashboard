module RewardGraph.Grid exposing (circle, offset, point, rectangle)

import Circle2d exposing (Circle2d)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)


gridSize =
    15
 

offset : Float -> Float
offset o =
    o * gridSize


offsetPx : Float -> Quantity Float Pixels
offsetPx o =
    Pixels.pixels <| offset o


rectangle : Float -> Float -> Float -> Float -> Rectangle2d Pixels coordinates
rectangle x y w h =
    Rectangle2d.with
        { x1 = offsetPx x
        , x2 = offsetPx (x + w)
        , y1 = offsetPx y
        , y2 = offsetPx (y + h)
        }


circle : Float -> Float -> Float -> Circle2d Pixels coordinates
circle x y r =
    Circle2d.withRadius (offsetPx r) (point x y)


point : Float -> Float -> Point2d Pixels coordinates
point x y =
    Point2d.xy (offsetPx x) (offsetPx y)
