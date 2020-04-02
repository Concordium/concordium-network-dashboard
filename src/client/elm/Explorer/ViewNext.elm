module Explorer.ViewNext exposing (..)

import Context exposing (Context)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Icons exposing (..)
import Palette exposing (withAlphaEl)


view : Context a -> Element msg
view ctx =
    viewContainer ctx
        (column [ width fill ]
            ([ viewHeader ctx "sdf308h20fsodufh2038f0sdh"
             , viewContentHeadline ctx
             ]
                ++ List.map (viewTransaction ctx) (List.range 0 10)
            )
        )


viewContainer : Context a -> Element msg -> Element msg
viewContainer ctx content =
    el
        [ height fill
        , width fill
        , Background.color ctx.palette.bg2
        , Border.rounded 6
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 15
            , color = rgba 0 0 0 0.1
            }
        ]
        content


viewHeader : Context a -> String -> Element msg
viewHeader ctx hash =
    row ([ width fill ] ++ bottomBorder ctx) [ viewBlockHash ctx hash, viewBlockStats ctx ]


viewBlockHash : Context a -> String -> Element msg
viewBlockHash ctx hash =
    el [ paddingXY 6 6 ]
        (row
            [ height (px 36)
            , Border.rounded 4
            , Background.color (withAlphaEl 0.1 ctx.palette.c2)
            , Font.color (withAlphaEl 0.3 ctx.palette.c2)
            , paddingXY 10 10
            ]
            [ el [ Font.color ctx.palette.c2 ] (text "378fa762")
            , text hash
            ]
        )


viewBlockStats : Context a -> Element msg
viewBlockStats ctx =
    el [ alignRight, paddingXY 20 6 ] <| text "2s"


bottomBorder ctx =
    [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
    , Border.color ctx.palette.bg1
    ]


viewContentHeadline : Context a -> Element msg
viewContentHeadline ctx =
    row ([ width fill, height (px 36) ] ++ bottomBorder ctx)
        [ el [ paddingXY 10 0, Font.color ctx.palette.fg3 ] <| text "CONTENT" ]


viewTransaction : Context a -> Int -> Element msg
viewTransaction ctx number =
    row ([ width fill, height (px 36) ] ++ bottomBorder ctx) []
