module Chain.Interpolate exposing (..)

import Chain.Flatten exposing (..)
import Circle2d exposing (Circle2d)
import Color.Manipulate
import Direction2d exposing (Direction2d)
import Interpolation exposing (Interpolator)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)


interpolateFloatQuantity :
    Quantity Float units
    -> Quantity Float units
    -> Interpolator (Quantity Float units)
interpolateFloatQuantity a b =
    Quantity.interpolateFrom a b


interpolatePoint2d :
    Point2d units coords
    -> Point2d units coords
    -> Interpolator (Point2d units coords)
interpolatePoint2d pointA pointB =
    Point2d.interpolateFrom pointA pointB


parabola : Float -> Float -> Float
parabola height t =
    height * (1 - ((2 * t - 1) ^ 2))


interpolatePoint2dParabolic :
    Float
    -> Point2d Pixels coords
    -> Point2d Pixels coords
    -> Interpolator (Point2d Pixels coords)
interpolatePoint2dParabolic yoffset pointA pointB =
    \t ->
        Point2d.interpolateFrom pointA pointB t
            |> Point2d.translateIn Direction2d.y (Pixels.pixels (parabola yoffset t))


interpolateCircle2d :
    Circle2d units coords
    -> Circle2d units coords
    -> Interpolator (Circle2d units coords)
interpolateCircle2d circleA circleB =
    Interpolation.map2 Circle2d.atPoint
        (interpolatePoint2d (Circle2d.centerPoint circleA) (Circle2d.centerPoint circleB))
        (Quantity.interpolateFrom (Circle2d.radius circleA) (Circle2d.radius circleB))


interpolateCircle2dParabolic :
    Float
    -> Circle2d Pixels coords
    -> Circle2d Pixels coords
    -> Interpolator (Circle2d Pixels coords)
interpolateCircle2dParabolic yoffset circleA circleB =
    Interpolation.map2 Circle2d.atPoint
        (interpolatePoint2dParabolic yoffset (Circle2d.centerPoint circleA) (Circle2d.centerPoint circleB))
        (Quantity.interpolateFrom (Circle2d.radius circleA) (Circle2d.radius circleB))


interpolateRectangle2d :
    Rectangle2d units coords
    -> Rectangle2d units coords
    -> Interpolator (Rectangle2d units coords)
interpolateRectangle2d rectA rectB =
    Interpolation.map2 Rectangle2d.from
        (interpolatePoint2d
            (Rectangle2d.interpolate rectA 0 0)
            (Rectangle2d.interpolate rectB 0 0)
        )
        (interpolatePoint2d
            (Rectangle2d.interpolate rectA 1 1)
            (Rectangle2d.interpolate rectB 1 1)
        )


interpolateBlock : DrawableBlock -> DrawableBlock -> Interpolator DrawableBlock
interpolateBlock blockA blockB =
    \t ->
        { blockB
            | rect = interpolateRectangle2d blockA.rect blockB.rect t
            , color = Interpolation.hsl blockA.color blockB.color t
            , fractionNodesAt = Interpolation.float blockA.fractionNodesAt blockB.fractionNodesAt t
        }


interpolateBlocks :
    List DrawableBlock
    -> List DrawableBlock
    -> Interpolator (List DrawableBlock)
interpolateBlocks =
    Interpolation.list
        { add =
            \newBlock ->
                interpolateBlock
                    { newBlock | color = Color.Manipulate.fadeOut 1 newBlock.color }
                    newBlock
        , remove =
            \oldBlock ->
                interpolateBlock
                    oldBlock
                    { oldBlock | color = Color.Manipulate.fadeOut 1 oldBlock.color }
        , change = interpolateBlock
        , id = .hash
        , combine = Interpolation.combineParallel
        }


interpolateConnector : DrawableConnector -> DrawableConnector -> Interpolator DrawableConnector
interpolateConnector connA connB =
    \t ->
        { connB
            | start = interpolatePoint2d connA.start connB.start t
            , end = interpolatePoint2d connA.end connB.end t
            , color = Interpolation.hsl connA.color connB.color t
        }


interpolateConnectors :
    List DrawableConnector
    -> List DrawableConnector
    -> Interpolator (List DrawableConnector)
interpolateConnectors =
    Interpolation.list
        { add =
            \new ->
                interpolateConnector
                    { new | color = Color.Manipulate.fadeOut 1 new.color }
                    new
        , remove =
            \old ->
                interpolateConnector
                    old
                    { old | color = Color.Manipulate.fadeOut 1 old.color }
        , change = interpolateConnector
        , id = .id
        , combine = Interpolation.combineParallel
        }


interpolateDrawableChain :
    DrawableChain
    -> DrawableChain
    -> Interpolator DrawableChain
interpolateDrawableChain chainA chainB =
    \t ->
        { blocks = interpolateBlocks chainA.blocks chainB.blocks t
        , connectors = interpolateConnectors chainA.connectors chainB.connectors t
        , width = chainB.width
        , height = chainB.height
        , viewBoxOffsetX = Interpolation.float chainA.viewBoxOffsetX chainB.viewBoxOffsetX t
        , numCollapsedBlocksX = chainB.numCollapsedBlocksX
        , numCollapsedBlocksY = chainB.numCollapsedBlocksY
        }
