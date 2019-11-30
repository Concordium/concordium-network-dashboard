module Chain.SvgView exposing (..)

import Chain.Api exposing (..)
import Chain.Spec exposing (spec)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


viewFlattenedChain : List (Positioned Block) -> Svg msg
viewFlattenedChain blocks =
    el
        ([ width (px 400), centerX, centerY ] ++ List.map (\b -> inFront (viewPositionedBlock b)) blocks)
        none



-- -- viewPositionedBlock : Positioned Block -> Svg msg


viewPositionedBlock positionedBlock =
    Keyed.el
        [ animateAll

        --, animateFromRight
        , moveRight <|
            toFloat positionedBlock.x
                * (spec.blockWidth + spec.gutterWidth)
        , moveDown <|
            toFloat positionedBlock.y
                * (spec.blockHeight + spec.gutterHeight + spec.nodeIndicatorHeight)
        , width (px <| round (spec.blockWidth + spec.gutterWidth))
        ]



-- ( positionedBlock.hash, viewBlock (unPositioned positionedBlock) )
