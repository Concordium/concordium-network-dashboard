module Explorer.View exposing (..)

import Context exposing (Context)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Explorer
import Explorer.Request exposing (..)
import Types exposing (..)


view ({ explorerModel } as ctx) =
    let
        block =
            stubBlock

        blockInfoStub_ =
            -- Explorer.Request.getBlockInfoStub_ "blah"
            Explorer.Request.blockInfoStub

        summary =
            Explorer.Request.getBlockSummaryStub_
    in
    case explorerModel.currentBlockInfo of
        Just blockInfo ->
            viewBlockLoaded ctx blockInfo summary

        Nothing ->
            viewBlockLoaded ctx blockInfoStub_ summary


viewBlockLoaded : Context a -> BlockInfo -> BlockSummary -> Element Msg
viewBlockLoaded ctx blockInfo blockSummary =
    column [ width fill, padding 30, spacing 10 ]
        [ row [ width fill, spacing 10 ]
            [ paragraph
                [ Background.color ctx.palette.bg2
                , padding 10
                , Border.rounded 5
                ]
                [ text <| "Height: " ++ String.fromInt blockInfo.blockHeight
                ]
            , paragraph
                [ alignLeft
                , width (fillPortion 1)
                , Background.color ctx.palette.bg2
                , padding 10
                , Border.rounded 5
                , onClick (ExplorerMsg (Explorer.RequestedBlockInfo blockInfo.blockParent))
                ]
                [ text <| "Parent: " ++ hashSnippet blockInfo.blockParent ]
            , paragraph [ centerX, width (fillPortion 1), Background.color ctx.palette.bg2, padding 10, Border.rounded 5 ]
                [ text <| "Block: " ++ hashSnippet blockInfo.blockHash ]
            , row [ alignRight, width (fillPortion 1), Background.color ctx.palette.bg2, padding 10, Border.rounded 5 ]
                [ paragraph [ Font.center ]
                    [ if blockInfo.finalized then
                        paragraph [ Font.color ctx.palette.success ] [ text "Finalized" ]

                      else
                        paragraph [ Font.color ctx.palette.c1 ] [ text "Pending" ]
                    ]
                ]
            ]
        , row [ width fill, spacing 10 ]
            [ paragraph [ Background.color ctx.palette.bg2, padding 10, Border.rounded 5 ] [ text <| "Slot: " ++ blockInfo.blockSlotTime ]
            , paragraph [ Background.color ctx.palette.bg2, padding 10, Border.rounded 5 ] [ text <| "Seen: " ++ blockInfo.blockReceiveTime ]
            ]
        , column []
            [ case blockInfo.transactionsSize of
                0 ->
                    paragraph [] [ text "This block has no transactions in it." ]

                _ ->
                    paragraph [] [ text "Todo: implement transaction listing" ]
            ]
        , column [ width fill, Background.color ctx.palette.bg2, padding 10, Border.rounded 5 ] <|
            List.map viewTransactionSummary blockSummary.transactionSummaries
        , column [] <| List.map viewSpecialEvent blockSummary.specialEvents
        ]


viewTransactionSummary : TransactionSummary -> Element msg
viewTransactionSummary s =
    let
        events =
            s.events
                |> List.map
                    viewEvent

        viewEvent e =
            row [ spacing 10 ]
                [ paragraph [] [ text e.tag ]
                , paragraph [] [ text <| String.fromInt e.amount ]
                , paragraph [] [ text <| hashSnippet <| showAddress e.from ]
                , paragraph [] [ text <| hashSnippet <| showAddress e.to ]
                ]

        showAddress a =
            case a of
                AddressAccount address ->
                    address

                AddressContract address ->
                    address

                AddressUnknown ->
                    "(Error: Address Failed)"
    in
    column []
        [ row [ spacing 10 ] [ text s.tipe, text <| hashSnippet s.sender, text <| hashSnippet s.hash ]
        , column [] events
        ]


viewSpecialEvent : SpecialEvent -> Element msg
viewSpecialEvent e =
    row []
        [ text <|
            "Reward: "
                ++ String.fromInt e.rewardamount
                ++ " Baker: "
                ++ String.fromInt e.bakerid
                ++ " Account: "
                ++ hashSnippet e.bakeraccount
        ]


blockInfoStub =
    { transactionsSize = 0
    , blockParent = "d06708ea234df3189aa212008d8f0a97ba68384482d25fbeebdc2c822421f8ff"
    , mintedAmountPerSlot = 100
    , totalEncryptedAmount = 0
    , blockHash = "957c2ae05d82e9ba30142e02cda3b3c2ab779329d359c665abc7059dbb88cc61"
    , finalized = True
    , totalAmount = 15000628024800
    , blockArriveTime = "2020-03-05T17:04:10.8763399Z"
    , blockReceiveTime = "2020-03-05T17:04:10.8763399Z"
    , transactionCount = 0
    , transactionEnergyCost = 0
    , blockSlot = 6280248
    , blockLastFinalized = "a0cdd5b7e51d83bef2d0ed55cb35cdc8c42280add22e9e59c893ecb492ff609a"
    , blockSlotTime = "2020-03-05T16:08:00Z"
    , blockHeight = 79
    , blockBaker = 2
    , executionCost = 0
    , centralBankAmount = 474
    }


hashSnippet hash =
    String.left 6 hash ++ "..."
