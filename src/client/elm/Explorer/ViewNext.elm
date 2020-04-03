module Explorer.ViewNext exposing (..)

import Chain.Flatten exposing (DrawableBlock)
import Context exposing (Context)
import Dashboard.Widgets exposing (remoteDataView)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Explorer.Request exposing (..)
import Icons exposing (..)
import Material.Icons.Sharp as MIcons
import Material.Icons.Types exposing (Coloring(..))
import Palette exposing (withAlphaEl)
import RemoteData exposing (RemoteData(..), WebData)


view : Context a -> WebData BlockInfo -> Element msg
view ctx remoteBlockInfo =
    viewContainer ctx
        (remoteDataView ctx.palette
            (\blockInfo ->
                column
                    [ width fill ]
                    ([ viewHeader ctx blockInfo
                     , viewContentHeadline ctx
                     ]
                        ++ List.map (viewTransaction ctx) (List.range 0 10)
                    )
            )
            remoteBlockInfo
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


viewHeader : Context a -> BlockInfo -> Element msg
viewHeader ctx blockInfo =
    row ([ width fill ] ++ bottomBorder ctx)
        [ viewBlockHash ctx blockInfo
        , viewParentLink ctx blockInfo
        , viewBlockStats ctx blockInfo
        ]


viewParentLink : Context a -> BlockInfo -> Element msg
viewParentLink ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo
                |> withAlphaEl 0.5
    in
    row [ Font.color color, paddingXY 30 0 ]
        [ el [] (html <| Icons.block_not_finalized 20)
        , el [] (html <| Icons.arrow_left 20)
        , el [ width (px 2) ] none
        ]


viewBlockHash : Context a -> BlockInfo -> Element msg
viewBlockHash ctx blockInfo =
    let
        ( short, remaining ) =
            ( String.left 4 blockInfo.blockHash
            , String.dropLeft 4 blockInfo.blockHash
            )

        ( icon, color ) =
            if blockInfo.finalized == True then
                ( html <| Icons.block_finalized 20, ctx.palette.c2 )

            else
                ( html <| Icons.block_not_finalized 20, ctx.palette.c1 )
    in
    el [ paddingXY 6 6 ]
        (row
            [ height (px 36)
            , Border.rounded 4
            , Background.color (withAlphaEl 0.1 color)
            , Font.color (withAlphaEl 0.3 color)
            , paddingXY 10 10
            ]
            [ el [] icon
            , el [ width (px 10) ] none
            , el [ Font.color color ] (text short)
            , text remaining
            ]
        )


blockColor : Context a -> BlockInfo -> Color
blockColor ctx blockInfo =
    if blockInfo.finalized == True then
        ctx.palette.c2

    else
        ctx.palette.c1


viewBlockStats : Context a -> BlockInfo -> Element msg
viewBlockStats ctx blockInfo =
    row [ alignRight, paddingXY 20 6, spacing 30 ]
        [ viewBlockHeight ctx blockInfo
        , viewSlotTime ctx blockInfo
        ]


viewSlotTime : Context a -> BlockInfo -> Element msg
viewSlotTime ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo
    in
    row
        [ height fill
        , spacing 10
        , Font.color color
        , alignRight
        ]
        [ el [ Font.color (withAlphaEl 0.5 <| color) ]
            (html <| Icons.time_stopwatch 20)
        , text "2m12s"
        ]


viewBlockHeight : Context a -> BlockInfo -> Element msg
viewBlockHeight ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo
    in
    row
        [ height fill
        , spacing 10
        , Font.color color
        , alignRight
        ]
        [ el [ moveUp 1, Font.color (withAlphaEl 0.5 <| color) ] (html <| Icons.blockheight 20)
        , text (blockInfo.blockHeight |> String.fromInt)
        ]


bottomBorder ctx =
    [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
    , Border.color ctx.palette.bg1
    ]


viewContentHeadline : Context a -> Element msg
viewContentHeadline ctx =
    row ([ width fill, height (px 46) ] ++ bottomBorder ctx)
        [ el [ paddingXY 10 0, Font.color ctx.palette.fg3 ] <| text "CONTENT" ]


viewTransaction : Context a -> Int -> Element msg
viewTransaction ctx number =
    row ([ width fill, height (px 46), paddingXY 10 0 ] ++ bottomBorder ctx)
        [ el [ paddingEach { top = 0, bottom = 0, left = 0, right = 20 } ]
            (html <| Icons.transaction 18)
        , el [ paddingXY 30 0 ] (text "1234.00")
        , viewBadge ctx
            { icon = el [] (html <| Icons.account_user 18)
            , label = text "12345678"
            }
        , el [ paddingXY 8 0 ] (html <| Icons.arrow_right 18)
        , viewBadge ctx
            { icon = el [] (html <| Icons.account_user 18)
            , label = text "a8b7e8ff"
            }
        , el [ alignRight ] (html <| Icons.status_success 20)
        ]


viewBadge : Context a -> { icon : Element msg, label : Element msg } -> Element msg
viewBadge ctx { icon, label } =
    row [ padding 5, spacing 4 ] [ icon, label ]
