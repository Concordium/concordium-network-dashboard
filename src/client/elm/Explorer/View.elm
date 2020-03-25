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
            -- viewBlockLoaded ctx blockInfo explorerModel.currentBlockSummary
            viewBlockLoaded ctx blockInfo (Just summary)

        Nothing ->
            viewBlockLoaded ctx blockInfoStub_ explorerModel.currentBlockSummary


viewBlockLoaded : Context a -> BlockInfo -> Maybe BlockSummary -> Element Msg
viewBlockLoaded ctx blockInfo blockSummaryM =
    column [ width fill, spacing 10 ]
        [ row [ width fill, spacing 10 ]
            [ paragraph [ centerX, width (fillPortion 1), Background.color ctx.palette.bg2, padding 10, Border.rounded 5 ]
                [ text <| "Block: " ++ hashSnippet blockInfo.blockHash ]
            , paragraph
                [ alignLeft
                , width (fillPortion 1)
                , Background.color ctx.palette.bg2
                , padding 10
                , Border.rounded 5
                , onClick (ExplorerMsg (Explorer.RequestedBlockInfo blockInfo.blockParent))
                , pointer
                ]
                [ text <| "Parent: " ++ hashSnippet blockInfo.blockParent ]
            , paragraph
                [ Background.color ctx.palette.bg2
                , padding 10
                , Border.rounded 5
                ]
                [ text <| "Height: " ++ String.fromInt blockInfo.blockHeight
                ]
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
            [ if blockInfo.transactionCount == 0 then
                paragraph [] [ text "This block has no transactions in it." ]

              else
                case blockSummaryM of
                    Just blockSummary ->
                        column [ spacing 10, width fill ]
                            [ column [] <| List.map (viewSpecialEvent ctx) blockSummary.specialEvents
                            , column [ width fill, spacing 10 ] <|
                                List.map (viewTransactionSummary ctx) blockSummary.transactionSummaries
                            ]

                    Nothing ->
                        paragraph [] [ text "No summary loaded" ]
            ]
        ]


viewTransactionSummary : Context a -> TransactionSummary -> Element msg
viewTransactionSummary ctx s =
    viewTransactionSummaryTransfer ctx s


viewTransactionSummaryTransfer : Context a -> TransactionSummary -> Element msg
viewTransactionSummaryTransfer ctx s =
    let
        events =
            s.events
                |> List.map
                    viewEvent

        viewEvent e =
            case e of
                TransactionEventTransfer eventTransfer ->
                    row [ spacing 10 ]
                        [ paragraph [] [ text <| showAddress eventTransfer.from ++ " -> " ++ showAddress eventTransfer.to ++ " " ++ String.fromInt eventTransfer.amount ]
                        ]

                TransactionEventAccountCreated eventAccountCreated ->
                    paragraph [] [ text <| "Account Created: " ++ eventAccountCreated.account ]

                TransactionEventCredentialDeployed eventCredentialDeployed ->
                    -- Is there value in showing this?
                    none

        -- paragraph [] [ text <| "Credentials Deployed: " ++ eventCredentialDeployed.regid ]
    in
    column [ Background.color ctx.palette.bg2, padding 10, Border.rounded 5 ]
        [ row [ spacing 10 ] [ text <| hashSnippet <| s.hash ]
        , column [] events
        ]


showAddress a =
    case a of
        AddressAccount address ->
            address

        AddressContract address ->
            address

        AddressUnknown ->
            "(Error: Address Failed)"


viewSpecialEvent : Context a -> SpecialEvent -> Element msg
viewSpecialEvent ctx e =
    row [ Background.color ctx.palette.bg2, padding 10, Border.rounded 5, width fill ]
        [ text <|
            -- ++ " Baker: "
            -- ++ String.fromInt e.bakerid
            "Baker: "
                ++ hashSnippet e.bakeraccount
                ++ " Reward: "
                ++ String.fromInt e.rewardamount
        ]



-- blockInfoStub =
--     { transactionsSize = 0
--     , blockParent = "d06708ea234df3189aa212008d8f0a97ba68384482d25fbeebdc2c822421f8ff"
--     , mintedAmountPerSlot = 100
--     , totalEncryptedAmount = 0
--     , blockHash = "957c2ae05d82e9ba30142e02cda3b3c2ab779329d359c665abc7059dbb88cc61"
--     , finalized = True
--     , totalAmount = 15000628024800
--     , blockArriveTime = "2020-03-05T17:04:10.8763399Z"
--     , blockReceiveTime = "2020-03-05T17:04:10.8763399Z"
--     , transactionCount = 0
--     , transactionEnergyCost = 0
--     , blockSlot = 6280248
--     , blockLastFinalized = "a0cdd5b7e51d83bef2d0ed55cb35cdc8c42280add22e9e59c893ecb492ff609a"
--     , blockSlotTime = "2020-03-05T16:08:00Z"
--     , blockHeight = 79
--     , blockBaker = 2
--     , executionCost = 0
--     , centralBankAmount = 474
--     }


hashSnippet hash =
    String.left 6 hash ++ "..."
