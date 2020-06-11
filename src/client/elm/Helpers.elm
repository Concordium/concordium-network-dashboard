module Helpers exposing (..)

import Context exposing (Context)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Html.Attributes exposing (style)
import Icons
import Palette exposing (withAlphaEl)


tooltip : (Element msg -> Attribute msg) -> Element Never -> Attribute msg
tooltip placement content =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , htmlAttribute <| style "transition" "opacity 200ms ease-out 200ms"
            , (placement << Element.map never) <|
                el
                    [ htmlAttribute (style "pointerEvents" "none")
                    , moveUp 12
                    , moveLeft 2
                    , alignLeft
                    ]
                    content
            ]
            none


stringTooltipAbove : Context a -> String -> Attribute msg
stringTooltipAbove ctx content =
    tooltip above
        (row
            [ paddingXY 12 8
            , Border.rounded 15
            , Background.color ctx.palette.bg3
            , Font.color ctx.palette.fg2
            , (behindContent << Element.map never)
                (el
                    [ htmlAttribute (style "pointerEvents" "none")
                    , alignBottom
                    , moveDown 5
                    , Font.color ctx.palette.bg3
                    ]
                    (html <| Icons.tooltip_arrow_round 24)
                )
            ]
            [ text content ]
        )


stringTooltipAboveWidget : Context a -> Element Never -> Attribute msg
stringTooltipAboveWidget ctx content =
    tooltip above
        (row
            [ paddingXY 12 8
            , moveLeft 130
            , Border.rounded 15
            , Background.color ctx.palette.bg3
            , Font.color ctx.palette.fg2
            , (behindContent << Element.map never)
                (el
                    [ htmlAttribute (style "pointerEvents" "none")
                    , alignBottom
                    , moveDown 5
                    , Font.color ctx.palette.bg3
                    ]
                    (html <| Icons.tooltip_arrow_round 24)
                )
            ]
            [ content ]
        )


stringTooltipAboveWithCopy : Context a -> String -> Attribute msg
stringTooltipAboveWithCopy ctx content =
    tooltip above
        (row
            [ paddingXY 12 8
            , Border.rounded 50
            , Background.color ctx.palette.bg3
            , Font.color ctx.palette.fg2
            , (behindContent << Element.map never)
                (el
                    [ htmlAttribute (style "pointerEvents" "none")
                    , moveDown 5
                    , Font.color ctx.palette.bg3
                    ]
                    (html <| Icons.tooltip_arrow_round 24)
                )
            , (behindContent << Element.map never)
                (el
                    [ moveDown 32
                    , moveRight 20
                    , Font.color (ctx.palette.fg1 |> withAlphaEl 0.3)
                    , Font.size 8
                    ]
                    (text "click to copy")
                )
            ]
            [ text content ]
        )


padding_ l t r b =
    paddingEach { bottom = b, left = l, right = r, top = t }
