module Explorer.ViewNext exposing (..)

import Chain.Flatten exposing (DrawableBlock)
import Context exposing (Context)
import Dashboard.Formatting as Formatting
import Dashboard.Widgets exposing (remoteDataView)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Explorer.Request exposing (..)
import Icons exposing (..)
import Iso8601
import Material.Icons.Sharp as MIcons
import Material.Icons.Types exposing (Coloring(..))
import Palette exposing (withAlphaEl)
import RemoteData exposing (RemoteData(..), WebData)


view : Context a -> WebData BlockInfo -> WebData BlockSummary -> Element msg
view ctx remoteBlockInfo remoteBlockSummary =
    viewContainer ctx
        (remoteDataView ctx.palette
            (\blockInfo ->
                let
                    summaries =
                        if blockInfo.transactionCount == 0 then
                            paragraph [ padding 10, Font.color ctx.palette.fg2 ]
                                [ text "This block has no transactions in it." ]

                        else
                            remoteDataView ctx.palette
                                (\blockSummary ->
                                    blockSummary.transactionSummaries
                                        |> List.map (viewTransaction ctx)
                                        |> column [ width fill ]
                                )
                                remoteBlockSummary
                in
                column
                    [ width fill ]
                    [ viewHeader ctx blockInfo
                    , viewContentHeadline ctx
                    , summaries
                    ]
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
        , Border.color (Palette.lightish ctx.palette.bg2)
        , Border.width 1
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

        slotTime =
            Formatting.formatTimeBetween blockInfo.blockSlotTime ctx.time
    in
    row
        [ height fill
        , spacing 10
        , Font.color color
        , alignRight
        ]
        [ el [ Font.color (withAlphaEl 0.5 <| color) ]
            (html <| Icons.time_stopwatch 20)
        , text slotTime
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


viewTransaction : Context a -> TransactionSummary -> Element msg
viewTransaction ctx txSummary =
    case txSummary.result of
        TransactionAccepted events ->
            let
                event =
                    List.head events
            in
            row
                ([ width fill
                 , height (px 46)
                 , paddingXY 10 0
                 , mouseOver [ Background.color <| Palette.lightish ctx.palette.bg2 ]
                 ]
                    ++ bottomBorder ctx
                )
                [ el [ paddingEach { top = 0, bottom = 0, left = 0, right = 20 } ]
                    (iconForEvent event)
                , el [ paddingXY 30 0, width (shrink |> minimum 100) ]
                    (el [ alignRight ] <| text <| String.fromInt txSummary.cost)
                , viewTransactionEvent ctx event
                , el [ alignRight ] (html <| Icons.status_success 20)
                ]

        TransactionRejected tag contents ->
            row
                ([ width fill
                 , height (px 46)
                 , paddingXY 10 0
                 , mouseOver [ Background.color <| Palette.lightish ctx.palette.bg2 ]
                 ]
                    ++ bottomBorder ctx
                )
                [ el [ paddingEach { top = 0, bottom = 0, left = 0, right = 20 } ]
                    (html <| Icons.status_failure 18)
                , el [ paddingXY 30 0, width (shrink |> minimum 100) ]
                    (el [ alignRight ] <| text <| String.fromInt txSummary.cost)
                , text <| "Rejected: " ++ tag ++ " " ++ contents
                , el [ alignRight ] (html <| Icons.status_failure 20)
                ]


iconForEvent event_ =
    case event_ of
        Just (TransactionEventAccountCreated event) ->
            html <| Icons.account_key_deployed 18

        Just (TransactionEventCredentialDeployed event) ->
            html <| Icons.account_credentials_deployed 18

        Just (TransactionEventTransfer event) ->
            html <| Icons.transaction 18

        Just (TransactionEventStakeDelegated event) ->
            html <| Icons.delegation_delegated 20

        Just (TransactionEventBakerAdded event) ->
            html <| Icons.baking_oven 18

        Nothing ->
            html <| Icons.transaction 18


viewTransactionEvent : Context a -> Maybe TransactionEvent -> Element msg
viewTransactionEvent ctx txEvent =
    case txEvent of
        Just (TransactionEventAccountCreated event) ->
            row []
                [ viewAddress ctx (AddressAccount event.account)
                ]

        Just (TransactionEventCredentialDeployed event) ->
            none

        Just (TransactionEventTransfer event) ->
            row []
                [ viewAddress ctx event.from
                , el [ paddingXY 8 0 ] (html <| Icons.arrow_right 18)
                , viewAddress ctx event.to
                ]

        Just (TransactionEventStakeDelegated event) ->
            -- type alias EventStakeDelegated =
            --     { tag : String
            --     , account : String
            --     , baker : Int
            --     }
            row []
                [ viewAddress ctx (AddressAccount event.account)
                ]

        Just (TransactionEventBakerAdded event) ->
            -- type alias EventBakerAdded =
            --     { tag : String
            --     , contents : Int
            --     }
            row []
                [ text <| String.fromInt event.contents
                ]

        Nothing ->
            none


viewBadge : Context a -> { icon : Element msg, label : Element msg } -> Element msg
viewBadge ctx { icon, label } =
    row [ padding 5, spacing 4 ] [ icon, label ]


viewAddress : Context a -> AccountInfo -> Element msg
viewAddress ctx addr =
    case addr of
        AddressAccount address ->
            viewBadge ctx
                { icon = el [] (html <| Icons.account_user 18)
                , label = text (String.left 8 address)
                }

        AddressContract address ->
            viewBadge ctx
                { icon = el [] (html <| Icons.smart_contract 18)
                , label = text (String.left 8 address)
                }

        AddressUnknown ->
            viewBadge ctx
                { icon = el [ Font.color ctx.palette.danger ] (html <| Icons.close 18)
                , label = text "(Error: Address Failed)"
                }
