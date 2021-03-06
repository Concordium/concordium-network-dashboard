module Tooltip exposing (..)

import Context exposing (Theme)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes exposing (style)
import Icons
import Palette exposing (uiToColor, withAlphaEl)


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
                    , alignLeft
                    ]
                    content
            ]
            none


downArrow : Theme a -> Element Never
downArrow ctx =
    el
        [ width fill
        , height (px 14)
        , Font.color ctx.palette.bg2
        , inFront
            (el [ paddingXY 4 0, moveUp 1 ] <|
                (html <| Icons.tooltip_arrow 14 (uiToColor ctx.palette.fg3))
            )
        ]
        none


tooltipStyle : Theme a -> List (Attribute msg)
tooltipStyle ctx =
    [ paddingXY 12 10
    , Border.rounded 5
    , Border.color ctx.palette.fg3
    , Border.width 1
    , Background.color ctx.palette.bg2
    , Font.color ctx.palette.fg2
    ]


stringTooltipAbove : Theme a -> String -> Attribute msg
stringTooltipAbove ctx content =
    tooltip above
        (column []
            [ row
                (tooltipStyle ctx)
                [ text content ]
            , downArrow ctx
            ]
        )


stringTooltipAboveWidget : Theme a -> Element Never -> Attribute msg
stringTooltipAboveWidget ctx content =
    tooltip above
        (column []
            [ column
                (tooltipStyle ctx ++ [ width (px 250), moveLeft 110 ])
                [ paragraph [] [ content ] ]
            , downArrow ctx
            ]
        )


stringTooltipAlignedRight : Theme a -> Element Never -> Attribute msg
stringTooltipAlignedRight ctx content =
    tooltip above
        (column []
            [ column
                (tooltipStyle ctx ++ [ width (px 250), moveLeft 230 ])
                [ paragraph [] [ content ] ]
            , downArrow ctx
            ]
        )


stringTooltipAboveWithCopy : Theme a -> String -> Attribute msg
stringTooltipAboveWithCopy ctx content =
    tooltip above
        (column []
            [ column
                (spacing 3 :: tooltipStyle ctx)
                [ paragraph [] [ text content ]
                , el
                    [ Font.size 11
                    , Font.color (ctx.palette.fg1 |> withAlphaEl 0.6)
                    ]
                    (text "Click to copy")
                ]
            , downArrow ctx
            ]
        )
