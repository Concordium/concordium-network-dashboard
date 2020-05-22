module Explorer.ViewNext exposing (..)

import Chain exposing (Msg(..))
import Chain.Flatten exposing (DrawableBlock)
import Context exposing (Context)
import Dashboard.Formatting as Formatting
import Dashboard.Widgets exposing (remoteDataView)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Explorer
import Explorer.Request exposing (..)
import Explorer.Stubs exposing (blockSummaryStubs)
import Html.Attributes exposing (style)
import Icons exposing (..)
import Iso8601
import Material.Icons.Sharp as MIcons
import Material.Icons.Types exposing (Coloring(..))
import Palette exposing (withAlphaEl)
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Time
import TimeHelpers
import Transaction.Event exposing (..)
import Transaction.Summary exposing (..)
import Types exposing (Msg(..))


view : Context a -> WebData BlockInfo -> WebData BlockSummary -> Element Msg
view ctx remoteBlockInfo remoteBlockSummary =
    column [ spacing 40, width fill ]
        [ viewContainer ctx
            (remoteDataView ctx.palette
                (\blockInfo ->
                    let
                        summaries =
                            remoteDataView ctx.palette
                                (\blockSummary ->
                                    let
                                        transactionSummaries =
                                            blockSummary.transactionSummaries
                                                |> List.map (viewTransaction ctx)

                                        specialEvents =
                                            blockSummary.specialEvents
                                                |> List.map (viewSpecialEvent ctx)

                                        finalizations =
                                            viewFinalizationData ctx blockSummary.finalizationData

                                        summaryItems =
                                            transactionSummaries ++ specialEvents ++ finalizations
                                    in
                                    if List.length summaryItems > 0 then
                                        column [ width fill ] summaryItems

                                    else
                                        column [ width fill ] [ text "This block has no transactions in it." ]
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

        -- , testStubs ctx
        ]


viewContainer : Context a -> Element Msg -> Element Msg
viewContainer ctx content =
    el
        [ height fill

        -- , width fill
        , width (fill |> maximum 1100)
        , centerX
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


viewHeader : Context a -> BlockInfo -> Element Msg
viewHeader ctx blockInfo =
    row ([ width fill, spacing 15, paddingXY 6 0 ] ++ bottomBorder ctx)
        [ viewBlockHash ctx blockInfo
        , viewParentLink ctx blockInfo
        , viewBlockStats ctx blockInfo
        ]


viewParentLink : Context a -> BlockInfo -> Element Msg
viewParentLink ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo
                |> withAlphaEl 0.5
    in
    row
        [ Font.color color
        , pointer

        -- @TODO figure out right way to do this
        -- , onClick (ExplorerMsg (Explorer.RequestedBlockInfo blockInfo.blockParent))
        , onClick (ChainMsg (BlockClicked blockInfo.blockParent))
        ]
        [ row [ stringTooltipAbove ctx "View parent block" ]
            [ el [] (html <| Icons.block_not_finalized 20)
            , el [] (html <| Icons.arrow_left 20)
            , el [ width (px 2) ] none
            , text <| String.left 8 blockInfo.blockParent
            ]
        ]


viewBlockHash : Context a -> BlockInfo -> Element Msg
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
    el [ paddingXY 0 6 ]
        (row
            [ height (px 36)
            , Border.rounded 4
            , Background.color (withAlphaEl 0.1 color)
            , Font.color (withAlphaEl 0.6 color)
            , paddingXY 10 10
            , stringTooltipAboveWithCopy ctx "Block hash"
            , onClick (CopyToClipboard blockInfo.blockHash)
            ]
            [ el [] icon
            , el [ width (px 10) ] none
            , paragraph []
                [ el [ Font.color color ] (text short)
                , text remaining
                ]
            ]
        )


blockColor : Context a -> BlockInfo -> Color
blockColor ctx blockInfo =
    if blockInfo.finalized == True then
        ctx.palette.c2

    else
        ctx.palette.c1


viewBlockStats : Context a -> BlockInfo -> Element Msg
viewBlockStats ctx blockInfo =
    row [ alignRight, spacing 15 ]
        [ viewBlockHeight ctx blockInfo
        , viewSlotTime ctx blockInfo
        ]


viewSlotTime : Context a -> BlockInfo -> Element Msg
viewSlotTime ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo

        slotTime =
            -- @TODO add timezone support?
            TimeHelpers.formatTime Time.utc blockInfo.blockSlotTime

        -- Formatting.formatTimeBetween blockInfo.blockSlotTime ctx.time
    in
    row
        [ height fill
        , spacing 10
        , Font.color color
        , alignRight
        , stringTooltipAboveWithCopy ctx "Slot time"
        , onClick (CopyToClipboard slotTime)
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
        , stringTooltipAbove ctx "Chain length"
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
    row
        ([ width fill
         , height (px 26)
         , paddingXY 10 0
         , spacing 15
         ]
            ++ bottomBorder ctx
        )
        [ el [ width (shrink |> minimum 30) ]
            (el [ Font.color ctx.palette.fg3 ] <| text "TYPE")
        , el [ width (shrink |> minimum 95) ]
            (el [ Font.color ctx.palette.fg3 ] <| text "SENDER")
        , row []
            [ el [ Font.color ctx.palette.fg3 ] <| text "CONTENT"
            ]
        , el [ width (shrink |> minimum 120), alignRight ]
            (el [ Font.color ctx.palette.fg3, alignRight ] <| text "COST")
        , el [ width (shrink |> minimum 105), alignRight ]
            (el [ Font.color ctx.palette.fg3 ] <| text "TX HASH")
        ]


viewTransaction : Context a -> TransactionSummary -> Element Msg
viewTransaction ctx txSummary =
    let
        sender =
            txSummary.sender
                |> Maybe.map AddressAccount

        softSenderFallback event_ =
            case event_ of
                TransactionEventAccountCreated event ->
                    el [ Font.color ctx.palette.fg3 ] <| viewAddress ctx (AddressAccount event.account)

                TransactionEventCredentialDeployed event ->
                    el [ Font.color ctx.palette.fg3 ] <| viewAddress ctx (AddressAccount event.account)

                _ ->
                    none
    in
    case txSummary.result of
        TransactionAccepted events ->
            let
                v event =
                    row
                        ([ width fill
                         , height (px 46)
                         , paddingXY 10 0
                         , spacing 15
                         , mouseOver [ Background.color <| Palette.lightish ctx.palette.bg2 ]
                         ]
                            ++ bottomBorder ctx
                        )
                        [ el [ width (shrink |> minimum 30) ]
                            (iconForEvent ctx event)
                        , el [ width (shrink |> minimum 95) ] <|
                            case sender of
                                Just address ->
                                    viewAddress ctx address

                                Nothing ->
                                    softSenderFallback event
                        , viewTransactionEvent ctx event txSummary
                        , el [ width (shrink |> minimum 120), alignRight ]
                            (el [ alignRight ] <| text <| String.fromInt txSummary.cost)
                        , el
                            [ alignRight
                            , stringTooltipAboveWithCopy ctx txSummary.hash
                            , onClick (CopyToClipboard txSummary.hash)
                            ]
                            (el [ alignRight ] <| text <| String.left 8 txSummary.hash)
                        , el [ alignRight ] (html <| Icons.status_success 20)
                        ]
            in
            events
                |> List.map v
                |> column [ width fill ]

        -- v (List.head events)
        TransactionRejected tag contents ->
            row
                ([ width fill
                 , height (px 46)
                 , paddingXY 10 0
                 , spacing 15
                 , mouseOver [ Background.color <| Palette.lightish ctx.palette.bg2 ]
                 ]
                    ++ bottomBorder ctx
                )
                [ row [ width (shrink |> minimum 30) ]
                    [ el [ Font.color ctx.palette.failure ]
                        (iconForTag ctx tag)
                    ]
                , el [ width (shrink |> minimum 95) ] <|
                    case sender of
                        Just address ->
                            viewAddress ctx address

                        Nothing ->
                            none
                , paragraph [ Font.color ctx.palette.failure ] <|
                    if contents /= "" then
                        [ text <| tag ++ ": " ++ contents ]

                    else
                        [ text <| tag ]
                , el [ width (shrink |> minimum 120), alignRight ]
                    (el [ alignRight ] <| text <| String.fromInt txSummary.cost)
                , el
                    [ alignRight
                    , stringTooltipAboveWithCopy ctx txSummary.hash
                    , onClick (CopyToClipboard txSummary.hash)
                    ]
                    (el [ alignRight ] <| text <| String.left 8 txSummary.hash)
                , el [ alignRight, Font.color ctx.palette.failure ] (html <| Icons.status_failure 20)
                ]


viewSpecialEvent : Context a -> SpecialEvent -> Element Msg
viewSpecialEvent ctx specialEvent =
    row
        ([ width fill
         , height (px 46)
         , paddingXY 10 0
         , spacing 15
         , mouseOver [ Background.color <| Palette.lightish ctx.palette.bg2 ]
         ]
            ++ bottomBorder ctx
        )
        [ row [ spacing 10, width (shrink |> minimum 30) ]
            [ el [ stringTooltipAbove ctx "Baking reward" ]
                (html <| Icons.baking_bread 20)
            ]
        , el [ width (shrink |> minimum 95), Font.color ctx.palette.fg3 ]
            (text "Chain")
        , row []
            [ text <| "Rewarded " ++ String.fromInt specialEvent.rewardAmount
            , arrowRight
            , viewAddress ctx
                (AddressAccount specialEvent.bakerAccount)
            , el [] <| text <| " (Baker: " ++ String.fromInt specialEvent.bakerId ++ ")"
            ]
        , el
            [ alignRight
            , width (shrink |> minimum 70)
            , Font.color ctx.palette.fg3
            ]
            (el [] <| text <| "n/a")
        , el [ alignRight ] (html <| Icons.status_success 20)
        ]


viewFinalizationData ctx finalizationData =
    case finalizationData of
        Just data ->
            [ row
                ([ width fill
                 , height (px 46)
                 , paddingXY 10 0
                 , spacing 15
                 , mouseOver [ Background.color <| Palette.lightish ctx.palette.bg2 ]
                 ]
                    ++ bottomBorder ctx
                )
                [ row [ spacing 10, width (shrink |> minimum 30) ]
                    [ el [ stringTooltipAbove ctx "Finalization event", Font.color ctx.palette.c2 ]
                        (html <| Icons.block_finalized 20)
                    ]
                , el [ width (shrink |> minimum 95), Font.color ctx.palette.fg3 ]
                    (text "Chain")
                , let
                    totalWeight =
                        data.finalizers |> List.map .weight |> List.sum

                    viewFinalizer f =
                        "Baker "
                            ++ String.fromInt f.bakerId
                            ++ " ("
                            ++ Round.round 2 (toFloat f.weight / toFloat totalWeight * 100)
                            ++ "%)"
                            ++ (if f.signed then
                                    " signed"

                                else
                                    ""
                               )
                  in
                  row []
                    [ text <| "Finalized "
                    , el
                        [ onClick (ChainMsg (BlockClicked data.blockPointer))
                        , pointer
                        ]
                      <|
                        text <|
                            String.left 8 data.blockPointer
                    , text " "
                    , el
                        [ Font.color (ctx.palette.c2 |> withAlphaEl 0.6)
                        , stringTooltipAbove ctx
                            ("Index: "
                                ++ String.fromInt data.index
                                ++ ", "
                                ++ "Delay: "
                                ++ String.fromInt data.delay
                                ++ "\n\n"
                                ++ "Committee:\n\n"
                                ++ (data.finalizers |> List.map viewFinalizer |> String.join "\n")
                            )
                        ]
                      <|
                        text "details"
                    ]
                , el
                    [ alignRight
                    , width (shrink |> minimum 70)
                    , Font.color ctx.palette.fg3
                    ]
                    (el [] <| text <| "n/a")
                , el [ alignRight ] (html <| Icons.status_success 20)
                ]
            ]

        Nothing ->
            []


iconForEvent ctx event_ =
    case event_ of
        TransactionEventAccountCreated event ->
            row [ spacing 10 ]
                [ el [ stringTooltipAbove ctx "Account creation" ]
                    (html <| Icons.account_key_deployed 18)
                ]

        TransactionEventCredentialDeployed event ->
            row [ spacing 10 ]
                [ el [ stringTooltipAbove ctx "Account credentials deployment" ]
                    (html <| Icons.account_credentials_deployed 18)
                ]

        TransactionEventTransfer event ->
            row [ spacing 10 ]
                [ el [ stringTooltipAbove ctx "Transfer" ]
                    (html <| Icons.transaction 18)
                ]

        TransactionEventStakeDelegated event ->
            row [ spacing 10 ]
                [ el [ stringTooltipAbove ctx "Stake delegation" ]
                    (html <| Icons.delegation_delegated 20)
                ]

        TransactionEventBakerAdded event ->
            row [ spacing 10 ]
                [ el [ stringTooltipAbove ctx "Baker addition" ]
                    (html <| Icons.baking_bread 20)
                ]

        TransactionEventModuleDeployed event ->
            row [ spacing 10 ]
                [ el [ stringTooltipAbove ctx "Module deployment" ]
                    (html <| Icons.smart_contract_deploy 20)
                ]

        TransactionEventContractInitialized event ->
            row [ spacing 10 ]
                [ el [ stringTooltipAbove ctx "Contract creation" ]
                    (html <| Icons.smart_contract_add_new 20)
                ]

        TransactionEventContractMessage event ->
            row [ spacing 10 ]
                [ el [ stringTooltipAbove ctx "Contract message" ]
                    (html <| Icons.smart_contract_message 20)
                ]


iconForTag ctx tag =
    case tag of
        "InvalidStakeDelegationTarget" ->
            el [ stringTooltipAbove ctx "Invalid delegation target" ]
                (html <| Icons.delegation_delegated 20)

        "InvalidBakerRemoveSource" ->
            el [ stringTooltipAbove ctx "Invalid baker remove source" ]
                (html <| Icons.baking_bread 20)

        "SerializationFailure" ->
            el [ stringTooltipAbove ctx "Unknown" ]
                (el [ paddingXY 6 0 ] <| text "?")

        _ ->
            el [ stringTooltipAbove ctx tag ]
                (el [ paddingXY 6 0 ] <| text "?")


viewTransactionEvent : Context a -> TransactionEvent -> TransactionSummary -> Element Msg
viewTransactionEvent ctx txEvent txSummary =
    case txEvent of
        TransactionEventAccountCreated event ->
            -- type alias EventAccountCreated =
            --     { tag : String
            --     , account : String
            --     }
            row
                []
                [ text <| "Created account"
                , arrowRight
                , viewAddress ctx (AddressAccount event.account)
                ]

        TransactionEventCredentialDeployed event ->
            -- type alias EventCredentialDeployed =
            --     { tag : String
            --     , regid : String
            --     , account : String
            --     }
            row
                []
                [ text <| "Deployed credentials"
                , arrowRight
                , viewAddress ctx (AddressAccount event.account)
                ]

        TransactionEventTransfer event ->
            -- type alias EventTransfer =
            --     { amount : Int
            --     , tag : String
            --     , to : AccountInfo
            --     , from : AccountInfo
            --     }
            row []
                [ text <| "Sent " ++ String.fromInt event.amount
                , arrowRight
                , viewAddress ctx event.to
                ]

        TransactionEventStakeDelegated event ->
            -- type alias EventStakeDelegated =
            --     { tag : String
            --     , account : String
            --     , baker : Int
            --     }
            row []
                [ text "Delegated"
                , arrowRight
                , viewAddress ctx
                    (AddressAccount event.account)
                , text <| " (Baker: " ++ String.fromInt event.baker ++ ")"
                ]

        TransactionEventBakerAdded event ->
            -- type alias EventBakerAdded =
            --     { tag : String
            --     , contents : Int
            --     }
            row []
                [ text <| "Added"
                , arrowRight
                , text <| "Baker " ++ String.fromInt event.contents
                ]

        TransactionEventModuleDeployed event ->
            -- type alias EventModuleDeployed =
            --     { tag : String
            --     , contents : String
            --     }
            row []
                [ text <| "Deployed module"
                , arrowRight
                , el
                    [ stringTooltipAboveWithCopy ctx event.contents
                    , onClick (CopyToClipboard event.contents)
                    ]
                  <|
                    text <|
                        String.left 8 event.contents
                ]

        TransactionEventContractInitialized event ->
            -- type alias EventContractInitialized =
            --     { tag : String
            --     , amount : Int
            --     , address : ContractAddress
            --     , name : Int
            --     , ref : String
            --     }
            el []
                (row []
                    [ text "Created contract"
                    , arrowRight
                    , viewAsAddressContract ctx event.address
                    ]
                )

        TransactionEventContractMessage event ->
            -- type alias EventContractMessage =
            --     { tag : String
            --     , amount : Int
            --     , address : ContractAddress
            --     , message : String
            --     }
            el []
                (row []
                    [ text "Sent"
                    , text <|
                        " ["
                            ++ String.fromInt event.amount
                            ++ " + "
                    , el
                        [ Font.color ctx.palette.c3
                        , stringTooltipAbove ctx event.message
                        ]
                      <|
                        text "message"
                    , text "]"
                    , el [ paddingXY 8 0 ] (html <| Icons.arrow_right 18)
                    , viewAsAddressContract ctx event.address
                    ]
                )


viewAsAddressContract ctx contractAddress =
    let
        content =
            "{\"index\":"
                ++ String.fromInt contractAddress.index
                ++ ",\"subindex\":"
                ++ String.fromInt contractAddress.subindex
                ++ "}"
    in
    el [ stringTooltipAboveWithCopy ctx content, onClick (CopyToClipboard content) ]
        (viewAddress ctx
            (AddressContract <|
                String.fromInt contractAddress.index
                    ++ "-"
                    ++ String.fromInt contractAddress.subindex
            )
        )


viewAddress : Context a -> AccountInfo -> Element Msg
viewAddress ctx addr =
    case addr of
        AddressAccount address ->
            row
                [ spacing 4
                , stringTooltipAboveWithCopy ctx address
                , onClick (CopyToClipboard address)
                ]
                [ el [] (html <| Icons.account_user 18)
                , text (String.left 8 address)
                ]

        AddressContract address ->
            row [ spacing 4 ]
                [ el [] (html <| Icons.smart_contract 18)
                , text address
                ]

        AddressUnknown ->
            row [ spacing 4 ]
                [ el [ Font.color ctx.palette.danger ] (html <| Icons.close 18)
                , text "(Error: Address Failed)"
                ]


arrowRight =
    el [ paddingXY 8 0 ] (html <| Icons.arrow_right 18)


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
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 2
                , color = ctx.palette.fg1 |> withAlphaEl 0.3
                , blur = 25
                }
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


stringTooltipAboveWithCopy : Context a -> String -> Attribute msg
stringTooltipAboveWithCopy ctx content =
    tooltip above
        (row
            [ paddingXY 12 8
            , Border.rounded 50
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 2
                , color = ctx.palette.fg1 |> withAlphaEl 0.3
                , blur = 25
                }
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


testStubs ctx =
    row []
        [ text "Test stubs: "
        , blockSummaryStubs
            |> List.map (loadStubButton ctx)
            |> row [ spacing 5 ]
        ]


loadStubButton ctx ( name, stub ) =
    row
        [ Font.color ctx.palette.c1
        , padding 10
        , Background.color ctx.palette.bg2
        , pointer
        , onClick (BlockSummaryStubSelected stub)
        , Border.rounded 5
        ]
        [ text name
        ]