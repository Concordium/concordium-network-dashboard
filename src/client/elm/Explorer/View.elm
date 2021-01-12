module Explorer.View exposing (..)

import Context exposing (Context)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Explorer exposing (Model)
import Explorer.Request exposing (..)
import Html
import Html.Attributes exposing (style)
import Icons exposing (..)
import Palette exposing (withAlphaEl)
import Set exposing (Set)
import String exposing (toLower)
import Svg exposing (Svg)
import Time
import TimeHelpers
import Tooltip exposing (..)
import Transaction.Event exposing (..)
import Transaction.Summary exposing (..)
import Types as T exposing (AccountAddress)
import Widgets exposing (arrowRight, remoteDataView)


type Msg
    = CopyToClipboard String
    | BlockClicked String
    | ToggleDetails Int Int


type alias SummaryItem =
    List SummaryItemEvent


bakingRewardAccountUpper =
    "Baking reward account"


bakingRewardAccountLower =
    toLower bakingRewardAccountUpper


finalizationRewardAccountUpper =
    "Finalization reward account"


finalizationRewardAccountLower =
    toLower finalizationRewardAccountUpper


type alias SummaryItemEvent =
    { content : Element Msg, details : Maybe (Element Msg) }


view : Context a -> Model -> Element Msg
view ctx model =
    column [ spacing 40, width fill ]
        [ viewContainer ctx
            (remoteDataView ctx.palette
                (\blockInfo ->
                    let
                        summaries =
                            remoteDataView ctx.palette
                                (\{ blockSummary, detailsDisplayed } ->
                                    let
                                        transactionSummaries =
                                            blockSummary.transactionSummaries
                                                |> List.concatMap (viewTransactionSummary ctx)

                                        specialEvents =
                                            blockSummary.specialEvents
                                                |> List.map (viewSpecialEvent ctx blockSummary.updates.chainParameters.rewardParameters)

                                        finalizations =
                                            viewFinalizationData ctx blockSummary.finalizationData

                                        summaryItems : List SummaryItem
                                        summaryItems =
                                            transactionSummaries :: specialEvents ++ [ finalizations ]

                                        viewWithDetails content details displayDetails itemIndex eventIndex =
                                            [ el
                                                ([ width fill
                                                 , onClick (ToggleDetails itemIndex eventIndex)
                                                 , pointer
                                                 , htmlAttribute <| style "transition" "border 200ms ease-in"
                                                 ]
                                                    ++ (if displayDetails then
                                                            [ Background.color <| Palette.lightish ctx.palette.bg2
                                                            , Border.color (Palette.lightish ctx.palette.bg2)
                                                            ]

                                                        else
                                                            []
                                                       )
                                                )
                                                content
                                            , el
                                                ([ width fill
                                                 , Background.color <| Palette.veryLight ctx.palette.bg2
                                                 , htmlAttribute <| style "transition" "max-height 200ms ease-in"
                                                 ]
                                                    ++ (if displayDetails then
                                                            [ htmlAttribute <| style "max-height" "1000px"
                                                            ]

                                                        else
                                                            [ htmlAttribute <| style "max-height" "0"
                                                            , htmlAttribute <| style "overflow-y" "hidden"
                                                            ]
                                                       )
                                                )
                                                details
                                            ]
                                    in
                                    if List.isEmpty summaryItems then
                                        column [ width fill ] [ text "This block has no transactions in it." ]

                                    else
                                        column [ width fill ] <|
                                            List.concat <|
                                                List.concat <|
                                                    List.indexedMap
                                                        (\itemIndex summaryItem ->
                                                            List.indexedMap
                                                                (\eventIndex summaryItemEvent ->
                                                                    case summaryItemEvent.details of
                                                                        Just details ->
                                                                            let
                                                                                displayDetails =
                                                                                    detailsDisplayed
                                                                                        |> Dict.get itemIndex
                                                                                        |> Maybe.withDefault Set.empty
                                                                                        |> Set.member eventIndex
                                                                            in
                                                                            viewWithDetails summaryItemEvent.content details displayDetails itemIndex eventIndex

                                                                        Nothing ->
                                                                            [ summaryItemEvent.content ]
                                                                )
                                                                summaryItem
                                                        )
                                                        summaryItems
                                )
                                model.blockSummary
                    in
                    column
                        [ width fill ]
                        [ viewHeader ctx blockInfo
                        , viewContentHeadline ctx
                        , summaries
                        ]
                )
                model.blockInfo
            )
        ]


viewContainer : Context a -> Element msg -> Element msg
viewContainer ctx content =
    el
        [ height fill
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
        [ viewParentLink ctx blockInfo
        , viewBlockHash ctx blockInfo
        , viewBlockStats ctx blockInfo
        ]


viewParentLink : Context a -> BlockInfo -> Element Msg
viewParentLink ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo
                |> withAlphaEl 0.5

        icon =
            blockIcon blockInfo 20
    in
    row
        [ Font.color color
        , pointer
        , onClick (BlockClicked blockInfo.blockParent)
        ]
        [ row [ stringTooltipAbove ctx "Go to parent block" ]
            [ el [] (html icon)
            , el [ width (px 2) ] none
            , el [ Font.color color ] (text <| String.left 4 blockInfo.blockParent)
            , el [] (html <| Icons.arrow_left 20)
            ]
        ]


viewBlockHash : Context a -> BlockInfo -> Element Msg
viewBlockHash ctx blockInfo =
    let
        ( short, remaining ) =
            ( String.left 4 blockInfo.blockHash
            , String.dropLeft 4 blockInfo.blockHash
            )

        icon =
            blockIcon blockInfo 20

        copyIcon =
            Icons.copy_to_clipboard 18

        color =
            blockColor ctx blockInfo
    in
    el [ paddingXY 0 6 ]
        (row
            [ height (px 36)
            , Border.rounded 4
            , Background.color (withAlphaEl 0.1 color)
            , Font.color (withAlphaEl 0.6 color)
            , paddingXY 10 10
            ]
            [ el [] (html icon)
            , el [ width (px 10) ] none
            , el [ stringTooltipAbove ctx "Block hash" ]
                (paragraph []
                    [ el [ Font.color color ] (text short)
                    , text remaining
                    ]
                )
            , el [ width (px 10) ] none
            , el
                [ stringTooltipAbove ctx "Copy to clipboard"
                , pointer
                , onClick (CopyToClipboard blockInfo.blockHash)
                ]
                (html copyIcon)
            ]
        )


blockColor : Context a -> BlockInfo -> Color
blockColor ctx blockInfo =
    if blockInfo.finalized == True then
        ctx.palette.c2

    else
        ctx.palette.c1


blockIcon : BlockInfo -> Float -> Svg msg
blockIcon blockInfo =
    if blockInfo.finalized == True then
        Icons.block_finalized

    else
        Icons.block_not_finalized


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
        ]
        [ el [ Font.color (withAlphaEl 0.5 <| color) ]
            (html <| Icons.time_stopwatch 20)
        , el
            [ Font.color color
            , stringTooltipAbove ctx "Slot time"
            ]
            (text slotTime)
        , el
            [ Font.color (withAlphaEl 0.5 <| color)
            , stringTooltipAbove ctx "Copy to clipboard"
            , pointer
            , onClick (CopyToClipboard slotTime)
            ]
            (html <| Icons.copy_to_clipboard 18)
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


bottomBorder : Context a -> List (Attribute msg)
bottomBorder ctx =
    [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
    , Border.color ctx.palette.bg1
    ]


viewContentCells : { tipe : Element msg, sender : Element msg, event : Element msg, cost : Element msg, txHash : Element msg } -> List (Element msg)
viewContentCells content =
    [ el [ width (shrink |> minimum 30) ] content.tipe
    , el [ width (shrink |> minimum 95) ] content.sender
    , el [ width fill ] content.event
    , el [ width (shrink |> minimum 120), alignRight ] content.cost
    , el [ width (shrink |> minimum 105), alignRight ] content.txHash
    ]


viewItemRow : Context a -> List (Attribute msg) -> List (Element msg) -> Element msg
viewItemRow ctx attrs children =
    row
        ([ width fill
         , height (px 46)
         , paddingXY 10 0
         , spacing 15
         , mouseOver [ Background.color <| Palette.lightish ctx.palette.bg2 ]
         ]
            ++ bottomBorder ctx
            ++ attrs
        )
        children


viewContentHeadline : Context a -> Element msg
viewContentHeadline ctx =
    viewItemRow ctx [ height (px 26), mouseOver [] ] <|
        viewContentCells
            { tipe = el [ Font.color ctx.palette.fg1 ] <| text ""
            , sender = el [ Font.color ctx.palette.fg1 ] <| text "SENDER"
            , event = el [ Font.color ctx.palette.fg1 ] <| text "EVENTS"
            , cost = el [ Font.color ctx.palette.fg1, alignRight ] <| text "COST"
            , txHash = el [ Font.color ctx.palette.fg1 ] <| text "TX HASH"
            }


type alias TransactionEventItem =
    { content : List (Element Msg)
    , details : Maybe (Element Msg)
    }


viewTransactionSummary : Context a -> TransactionSummary -> SummaryItem
viewTransactionSummary ctx txSummary =
    let
        typeDecription =
            typeDescriptionTransactionSummaryType txSummary.tipe

        icon =
            iconFromTypeDescription ctx typeDecription

        senderView event =
            case txSummary.sender of
                Just address ->
                    viewAddress ctx <| T.AddressAccount address

                Nothing ->
                    softSenderFallback event

        softSenderFallback event_ =
            case event_ of
                TransactionEventAccountCreated event ->
                    el [ Font.color ctx.palette.fg1 ] <| viewAddress ctx (T.AddressAccount event.account)

                TransactionEventCredentialDeployed event ->
                    el [ Font.color ctx.palette.fg1 ] <| viewAddress ctx (T.AddressAccount event.account)

                TransactionEventUpdateEnqueued _ ->
                    el [ Font.color ctx.palette.fg1 ] <| text "Foundation"

                _ ->
                    none

        viewMainEventItem event =
            let
                item =
                    viewTransactionEvent ctx event
            in
            { content =
                viewItemRow ctx [] <|
                    viewContentCells
                        { tipe = icon
                        , sender = senderView event
                        , event = row [] item.content
                        , cost = el [ alignRight ] <| text <| T.amountToString txSummary.cost
                        , txHash =
                            row [ width fill ]
                                [ el
                                    [ stringTooltipAboveWithCopy ctx txSummary.hash
                                    , pointer
                                    , onClick (CopyToClipboard txSummary.hash)
                                    ]
                                    (el [ alignRight ] <| text <| String.left 8 txSummary.hash)
                                , el [ alignRight ] (html <| Icons.status_success 20)
                                ]
                        }
            , details = item.details
            }

        viewSubEvent event =
            let
                item =
                    viewTransactionEvent ctx event
            in
            { content =
                viewItemRow ctx [] <|
                    viewContentCells
                        { tipe = none
                        , sender = none
                        , event =
                            row [] <|
                                el [ width (px 20) ] none
                                    :: item.content
                        , cost = none
                        , txHash = none
                        }
            , details = item.details
            }
    in
    case txSummary.result of
        TransactionAccepted events ->
            case events of
                [] ->
                    []

                mainEvent :: subEvents ->
                    viewMainEventItem mainEvent :: List.map viewSubEvent subEvents

        TransactionRejected rejectReason ->
            let
                item =
                    rejectionToItem ctx rejectReason
            in
            [ { content =
                    viewItemRow ctx [] <|
                        viewContentCells
                            { tipe = icon
                            , sender = Maybe.withDefault none <| Maybe.map (viewAddress ctx << T.AddressAccount) txSummary.sender
                            , event = paragraph [ Font.color ctx.palette.failure, width fill ] <| (text <| typeDecription.short ++ " failed: ") :: item.content
                            , cost = el [ alignRight ] <| text <| T.amountToString txSummary.cost
                            , txHash =
                                row [ width fill ]
                                    [ el
                                        [ stringTooltipAboveWithCopy ctx txSummary.hash
                                        , pointer
                                        , onClick (CopyToClipboard txSummary.hash)
                                        ]
                                        (el [ alignRight ] <| text <| String.left 8 txSummary.hash)
                                    , el [ alignRight, Font.color ctx.palette.failure ] (html <| Icons.status_failure 20)
                                    ]
                            }
              , details = Nothing
              }
            ]


type alias TypeDescription =
    { icon : Element Msg

    {- |Short description

       It is used to display as the intention of the transaction, and is sometimes appended with ' failed:'.
    -}
    , short : String
    }


iconFromTypeDescription : Context a -> TypeDescription -> Element Msg
iconFromTypeDescription ctx tyDesc =
    el [ stringTooltipAbove ctx tyDesc.short ] tyDesc.icon


typeDescriptionTransactionSummaryType : TransactionSummaryType -> TypeDescription
typeDescriptionTransactionSummaryType transactionSummaryType =
    case transactionSummaryType of
        AccountTransaction ty ->
            typeDescriptionAccountTransactionType ty

        CredentialDeploymentTransaction ty ->
            typeDescriptionCredentialType ty

        UpdateTransaction ty ->
            typeDescriptionUpdateType ty


typeDescriptionAccountTransactionType : AccountTransactionType -> TypeDescription
typeDescriptionAccountTransactionType accountTransactionType =
    case accountTransactionType of
        DeployModule ->
            { icon = html <| Icons.smart_contract_deploy 20, short = "Deploy module" }

        InitContract ->
            { icon = html <| Icons.smart_contract_add_new 20, short = "Initialize contract" }

        Update ->
            { icon = html <| Icons.smart_contract_message 20, short = "Update contract" }

        Transfer ->
            { icon = html <| Icons.transaction 18, short = "Transfer" }

        AddBaker ->
            { icon = html <| Icons.baking_bread 20, short = "Add baker" }

        RemoveBaker ->
            { icon = html <| Icons.baking_bread 20, short = "Remove baker" }

        UpdateBakerStake ->
            { icon = html <| Icons.baking_bread 20, short = "Update baker stake" }

        UpdateBakerRestakeEarnings ->
            { icon = html <| Icons.baking_bread 20, short = "Change baker restake earnings" }

        UpdateBakerKeys ->
            { icon = html <| Icons.baking_bread 20, short = "Update baker keys" }

        UpdateAccountKeys ->
            { icon = html <| Icons.account_key_deployed 18, short = "Update account keys" }

        AddAccountKeys ->
            { icon = html <| Icons.account_key_deployed 18, short = "Add account keys" }

        RemoveAccountKeys ->
            { icon = html <| Icons.account_key_deployed 18, short = "Remove account keys" }

        EncryptedAmountTransfer ->
            { icon = html <| Icons.shield 20, short = "Shielded transfer" }

        TransferToEncrypted ->
            { icon = html <| Icons.shield 20, short = "Shield amount" }

        TransferToPublic ->
            { icon = html <| Icons.shield 20, short = "Unshield amount" }

        TransferWithSchedule ->
            { icon = html <| Icons.transaction 18, short = "Transfer with schedule" }

        Malformed ->
            { icon = el [ paddingXY 6 0 ] <| text "?", short = "Serialization" }


typeDescriptionCredentialType : CredentialType -> TypeDescription
typeDescriptionCredentialType credentialType =
    case credentialType of
        Initial ->
            { icon = html <| Icons.account_credentials_deployed 18, short = "Deploy initial credential" }

        Normal ->
            { icon = html <| Icons.account_credentials_deployed 18, short = "Deploy normal credential" }


typeDescriptionUpdateType : UpdateType -> TypeDescription
typeDescriptionUpdateType updateType =
    { icon = html <| Icons.system_cog 20
    , short = "Enqueue chain update"
    }


rejectionToItem : Context a -> RejectReason -> { content : List (Element Msg), details : Maybe (Element Msg) }
rejectionToItem ctx reason =
    case reason of
        ModuleNotWF ->
            { content = [ text "Smart contract module failed to typecheck" ]
            , details = Nothing
            }

        ModuleHashAlreadyExists moduleRef ->
            { content = [ text "A module with the hash ", text moduleRef, text " already exists" ]
            , details = Nothing
            }

        InvalidAccountReference addr ->
            { content = [ text "The account ", viewAddress ctx <| T.AddressAccount addr, text " does not exists" ]
            , details = Nothing
            }

        InvalidModuleReference moduleRef ->
            { content = [ text "The module ", text moduleRef, text " does not exists" ]
            , details = Nothing
            }

        InvalidContractAddress contractAddr ->
            { content = [ text "No smart contract instance exists with address ", viewAddress ctx <| T.AddressContract contractAddr ]
            , details = Nothing
            }

        ReceiverAccountNoCredential addr ->
            { content = [ text "The receiving account ", viewAddress ctx <| T.AddressAccount addr, text " has no valid credential" ]
            , details = Nothing
            }

        ReceiverContractNoCredential addr ->
            { content = [ text "The receiving smart contract instance (", viewAddress ctx <| T.AddressContract addr, text ") has no valid credential" ]
            , details = Nothing
            }

        AmountTooLarge account amount ->
            { content = [ text "The sending account ", viewAddress ctx <| account, text " has insufficient funds" ]
            , details = Nothing
            }

        SerializationFailure ->
            { content = [ text "The transaction body was malformed" ]
            , details = Nothing
            }

        OutOfEnergy ->
            { content = [ text "The transaction did not contain enough energy" ]
            , details = Nothing
            }

        Rejected ->
            { content = [ text "Rejected by contract logic" ]
            , details = Nothing
            }

        NonExistentRewardAccount addr ->
            { content = [ text "The designated reward account ", viewAddress ctx <| T.AddressAccount addr, text " does not exist" ]
            , details = Nothing
            }

        InvalidProof ->
            { content = [ text "Proof that the baker owns relevant private keys is not valid" ]
            , details = Nothing
            }

        InvalidInitMethod moduleRef initName ->
            { content = [ text "No contract '", text initName, text "' found in module ", text moduleRef ]
            , details = Nothing
            }

        InvalidReceiveMethod moduleRef receiveName ->
            { content = [ text "No receive function '", text receiveName.functionName, text " of contract '", text receiveName.contractName, text "' found in module ", text moduleRef ]
            , details = Nothing
            }

        RuntimeFailure ->
            { content = [ text "Runtime failure when executing smart contract" ]
            , details = Nothing
            }

        DuplicateAggregationKey _ ->
            { content = [ text "Duplicate aggregation key" ]
            , details = Nothing
            }

        NonExistentAccountKey ->
            { content = [ text "Encountered index to which no account key belongs when removing or updating keys" ]
            , details = Nothing
            }

        KeyIndexAlreadyInUse ->
            { content = [ text "The requested key index is already in use" ]
            , details = Nothing
            }

        InvalidAccountKeySignThreshold ->
            { content = [ text "The requested sign threshold would exceed the number of keys on the account" ]
            , details = Nothing
            }

        InvalidEncryptedAmountTransferProof ->
            { content = [ text "The shielded amount transfer has an invalid proof" ]
            , details = Nothing
            }

        EncryptedAmountSelfTransfer _ ->
            { content = [ text "An shielded amount transfer from the account to itself is not allowed" ]
            , details = Nothing
            }

        InvalidTransferToPublicProof ->
            { content = [ text "The shielding has an invalid proof" ]
            , details = Nothing
            }

        InvalidIndexOnEncryptedTransfer ->
            { content = [ text "The provided shielded transfer index is out of bounds" ]
            , details = Nothing
            }

        ZeroScheduledAmount ->
            { content = [ text "Attempt to transfer 0 GTU with schedule" ]
            , details = Nothing
            }

        NonIncreasingSchedule ->
            { content = [ text "Attempt to transfer amount with non-increasing schedule" ]
            , details = Nothing
            }

        FirstScheduledReleaseExpired ->
            { content = [ text "The first scheduled release is in the past" ]
            , details = Nothing
            }

        ScheduledSelfTransfer _ ->
            { content = [ text "Attempt to transfer from account A to A with schedule" ]
            , details = Nothing
            }

        AlreadyABaker bakerId ->
            { content = [ text "Baker with ID ", text <| String.fromInt bakerId, text " already exists" ]
            , details = Nothing
            }

        NotABaker addr ->
            { content = [ text "Account ", viewAddress ctx <| T.AddressAccount addr, text " is not a baker" ]
            , details = Nothing
            }

        InsufficientBalanceForBakerStake ->
            { content = [ text "Sender account has insufficient balance to cover the requested stake" ]
            , details = Nothing
            }

        BakerInCooldown ->
            { content = [ text "Request to make change to the baker while the baker is in the cooldown period" ]
            , details = Nothing
            }


asPercentage : Float -> String
asPercentage n =
    String.left 5 (String.fromFloat (T.roundTo 4 n * 100)) ++ "%"


viewHeaderBox : Context a -> Color -> String -> Element msg -> Element msg
viewHeaderBox ctx color header content =
    column [ Border.width 1, Border.rounded 5, Border.color color ]
        [ el
            [ padding 10
            , width fill
            , Font.extraBold
            , Font.color ctx.palette.bg1
            , Font.center
            , Background.color color
            , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
            ]
          <|
            text header
        , el
            [ padding 10
            , width fill
            , Font.center
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            ]
            content
        ]


viewSpecialAccount : Context a -> Color -> String -> T.Amount -> Element Msg
viewSpecialAccount ctx color name amount =
    viewHeaderBox ctx color name <| text <| T.amountToString amount


viewPlusAmount : Context a -> T.Amount -> Element Msg
viewPlusAmount ctx amount =
    el [ Font.color ctx.palette.success, Font.center, width fill ] <| text <| "+ " ++ T.amountToString amount


type alias BarPart =
    { color : Color
    , percentage : Float
    , hint : String
    }


viewBar : Context a -> List BarPart -> Element msg
viewBar ctx parts =
    column [ width fill ]
        [ row [ width fill ] <|
            List.map
                (\p ->
                    el
                        [ width (fillPortion <| round (p.percentage * 100))
                        , padding 4
                        , Font.center
                        , Font.color p.color
                        , alignBottom
                        ]
                    <|
                        text p.hint
                )
                parts
        , row [ width fill ] <|
            List.map
                (\p ->
                    el
                        [ width (fillPortion <| round (p.percentage * 100))
                        , padding 4
                        , Font.center
                        , Font.color p.color
                        ]
                    <|
                        text (asPercentage p.percentage)
                )
                parts
        , row [ width fill, Border.rounded 5, clip, Border.width 1, Border.color ctx.palette.bg1 ] <|
            List.map
                (\p ->
                    el
                        [ width (fillPortion <| round (p.percentage * 100))
                        , Background.color p.color
                        , Font.color ctx.palette.bg1
                        , padding 5
                        , Font.center
                        ]
                    <|
                        Element.none
                )
                parts
        ]


viewDetailRow : List (Element msg) -> List (Element msg) -> Element msg
viewDetailRow l r =
    row [ width fill, spacing 20, padding 20 ] [ column [ width (fillPortion 2), spacing 20 ] l, column [ width (fillPortion 3), spacing 20 ] r ]


viewSpecialEvent : Context a -> RewardParameters -> SpecialEvent -> SummaryItem
viewSpecialEvent ctx rewardParameters specialEvent =
    let
        item =
            case specialEvent of
                SpecialEventBakingRewards event ->
                    { tooltip = "Baking rewards"
                    , icon = Icons.coin_gtu 20
                    , content =
                        row [ spacing 10 ]
                            [ text <| "Distributed " ++ bakingRewardAccountLower ]
                    , details =
                        let
                            bakerRewardList =
                                Dict.toList event.bakerRewards

                            bakingAccountDistributed =
                                T.unsafeSumAmounts <| List.map (\( _, amount ) -> amount) bakerRewardList

                            bakingAccountTotal =
                                T.unsafeAddAmounts event.remainder bakingAccountDistributed
                        in
                        viewDetailRow
                            [ paragraph [] [ text <| "Every epoch, the " ++ bakingRewardAccountLower ++ " is distributed among all bakers during the epoch." ]
                            , el [ centerX ] <| viewSpecialAccount ctx ctx.palette.c1 bakingRewardAccountUpper bakingAccountTotal
                            , paragraph [] [ text "The amount is distributed according to the share of blocks a baker have baked during the epoch." ]
                            , paragraph [] [ text <| "Some amount of GTU might be left because of rounding, these are left in the " ++ bakingRewardAccountLower ++ " for the next epoch." ]
                            , el [ centerX ] <| viewSpecialAccount ctx ctx.palette.c1 bakingRewardAccountUpper event.remainder
                            ]
                            [ viewTable ctx
                                { data = Dict.toList event.bakerRewards
                                , columns =
                                    [ { header = text "Baker"
                                      , width = fill
                                      , view = \i ( account, _ ) -> el [ centerX ] <| viewAddress ctx <| T.AddressAccount account
                                      }
                                    , { header = text "Share of baked blocks"
                                      , width = fill
                                      , view = \i ( _, amount ) -> text <| asPercentage <| Maybe.withDefault 0 <| Maybe.map2 (\n d -> n / d) (T.amountToFloat amount) (T.amountToFloat bakingAccountDistributed)
                                      }
                                    , { header = text "Reward"
                                      , width = fill
                                      , view = \i ( _, amount ) -> text <| T.amountToString amount
                                      }
                                    ]
                                }
                            ]
                    }

                SpecialEventMint event ->
                    { tooltip = "Minting"
                    , icon = Icons.minting_leaves 20
                    , content = el [ spacing 10 ] <| text "Distributed minted GTU "
                    , details =
                        let
                            foundationMintFraction =
                                1 - rewardParameters.mintDistribution.bakingReward - rewardParameters.mintDistribution.finalizationReward

                            mintTotal =
                                T.unsafeSumAmounts [ event.mintPlatformDevelopmentCharge, event.mintBakingReward, event.mintFinalizationReward ]
                        in
                        column [ spacing 25, width fill ]
                            [ viewDetailRow
                                [ paragraph [] [ text "Every block introduces an amount of minted GTU." ] ]
                                []
                            , viewDetailRow
                                [ paragraph []
                                    [ text "The amount depends on the number of slots since the last block, as each slot adds "
                                    , text <| String.fromFloat rewardParameters.mintDistribution.mintPerSlot
                                    , text " of GTU."
                                    ]
                                ]
                                [ el [ centerX ] <| viewSpecialAccount ctx ctx.palette.fg1 "Minted this block" mintTotal
                                ]
                            , viewDetailRow [ paragraph [] [ text "These GTU are distributed among special accounts for maintaining the blockchain and for rewarding bakers and finalizers. " ] ]
                                [ viewBar ctx
                                    [ { color = ctx.palette.c1, percentage = rewardParameters.mintDistribution.bakingReward, hint = bakingRewardAccountUpper }
                                    , { color = ctx.palette.c2, percentage = rewardParameters.mintDistribution.finalizationReward, hint = finalizationRewardAccountUpper }
                                    , { color = ctx.palette.fg2, percentage = foundationMintFraction, hint = "Foundation" }
                                    ]
                                ]
                            , viewDetailRow
                                []
                                [ row [ spaceEvenly, centerX, spacing 30 ]
                                    [ viewHeaderBox ctx ctx.palette.c1 bakingRewardAccountUpper <| viewPlusAmount ctx event.mintBakingReward
                                    , viewHeaderBox ctx ctx.palette.c2 finalizationRewardAccountUpper <| viewPlusAmount ctx event.mintFinalizationReward
                                    , viewHeaderBox ctx ctx.palette.fg2 "Foundation" <| viewPlusAmount ctx event.mintPlatformDevelopmentCharge
                                    ]
                                ]
                            ]
                    }

                SpecialEventFinalizationRewards event ->
                    { tooltip = "Rewarded finalizers"
                    , icon = Icons.coin_gtu 20
                    , content = row [ spacing 10 ] [ text <| "Distributed " ++ finalizationRewardAccountLower ]
                    , details =
                        let
                            finalizationAccountDistributed =
                                T.unsafeSumAmounts <| List.map (\( _, amount ) -> amount) <| Dict.toList event.finalizationRewards

                            finalizationAccountTotal =
                                T.unsafeAddAmounts event.remainder finalizationAccountDistributed
                        in
                        viewDetailRow
                            [ paragraph [] [ text <| "Every time a finalization proof is included in a block the " ++ finalizationRewardAccountLower ++ " distributes a reward among the finalizers. The reward is proportional to the finalizers' share of finalization stake." ]
                            , el [ centerX ] <| viewSpecialAccount ctx ctx.palette.c2 finalizationRewardAccountUpper finalizationAccountTotal
                            , paragraph [] [ text <| "The remaining GTU (which does not distribute evenly among the finalizers) stays in the " ++ finalizationRewardAccountLower ++ "." ]
                            , el [ centerX ] <| viewSpecialAccount ctx ctx.palette.c2 finalizationRewardAccountUpper event.remainder
                            ]
                            [ viewTable ctx
                                { data = Dict.toList event.finalizationRewards
                                , columns =
                                    [ { header = text "Finalizer"
                                      , width = fill
                                      , view = \i ( account, _ ) -> el [ centerX ] <| viewAddress ctx <| T.AddressAccount account
                                      }
                                    , { header = text "Finalizer stake"
                                      , width = fill
                                      , view = \i ( _, amount ) -> text <| asPercentage <| Maybe.withDefault 0 <| Maybe.map2 (\n d -> n / d) (T.amountToFloat amount) (T.amountToFloat finalizationAccountTotal)
                                      }
                                    , { header = text "Reward"
                                      , width = fill
                                      , view = \i ( _, amount ) -> text <| T.amountToString amount
                                      }
                                    ]
                                }
                            ]
                    }

                SpecialEventBlockReward event ->
                    { tooltip = "Block reward"
                    , icon = Icons.coin_gtu 20
                    , content =
                        row []
                            [ text <| "Rewarded " ++ T.amountToString event.bakerReward ++ " for baking this block"
                            , arrowRight
                            , viewAddress ctx (T.AddressAccount event.baker)
                            , text " "
                            ]
                    , details =
                        let
                            foundationTransactionFeeBlockReward =
                                1 - (rewardParameters.transactionFeeDistribution.baker + rewardParameters.transactionFeeDistribution.gasAccount)

                            bakerRewardAmountTransactionFee =
                                T.floorTo 6 <|
                                    rewardParameters.transactionFeeDistribution.baker
                                        * T.unsafeAmountToFloat event.transactionFees

                            bakerRewardAmountFixedGasAccount =
                                T.floorTo 6 <|
                                    rewardParameters.gasRewards.baker
                                        * T.unsafeAmountToFloat event.oldGASAccount

                            nonGasBakerAmount =
                                T.floorTo 6 <|
                                    T.unsafeAmountToFloat event.bakerReward
                                        - bakerRewardAmountTransactionFee
                                        - bakerRewardAmountFixedGasAccount

                            nonGasFraction =
                                let
                                    oldGasAccount =
                                        T.unsafeAmountToFloat event.oldGASAccount
                                in
                                if oldGasAccount == 0 then
                                    0

                                else
                                    nonGasBakerAmount / oldGasAccount
                        in
                        column [ width fill, spacing 30 ]
                            [ viewDetailRow
                                [ paragraph [] [ text "Each block rewards the baker baking it, this reward is paid by the transaction fees and the some fraction of the Gas Account." ] ]
                                []
                            , viewDetailRow
                                [ paragraph [] [ text "The transaction fees are distributed between the baker reward, a Gas Account and maintainance of the blockchain." ] ]
                                [ el [ centerX ] <| viewSpecialAccount ctx ctx.palette.fg1 "Transaction fees" event.transactionFees
                                , viewBar ctx
                                    [ { color = ctx.palette.c1, percentage = rewardParameters.transactionFeeDistribution.baker, hint = "Baker Reward" }
                                    , { color = ctx.palette.c3, percentage = rewardParameters.transactionFeeDistribution.gasAccount, hint = "Next Gas Account" }
                                    , { color = ctx.palette.fg2, percentage = foundationTransactionFeeBlockReward, hint = "Foundation" }
                                    ]
                                ]
                            , viewDetailRow
                                [ paragraph [] [ text "A block might include special transactions which do not cost a fee, these are account creation, chain updates and finalization proofs. Instead they are paid for by a fraction of the Gas Account." ]
                                , paragraph [] [ text "let ", italic "N", text " be defined to be" ]
                                , paragraph [ Font.center, Font.italic ] [ text "N = 1 - (1 - F)", super "f", text " · (1 - A)", super "a", text " · (1 - U)", super "u" ]
                                , paragraph []
                                    [ text "where "
                                    , italic "f"
                                    , text ", "
                                    , italic "a"
                                    , text " and "
                                    , italic "u"
                                    , text " are the number of finalization proofs, account creations and chain updates in this block respectively.\n"
                                    , italic "F"
                                    , text ", "
                                    , italic "A"
                                    , text " and "
                                    , italic "U"
                                    , text " are chain parameters."
                                    ]
                                , row [ centerX, spacing 30 ]
                                    [ paragraph [] [ italic "F", text " = ", text <| asPercentage rewardParameters.gasRewards.finalizationProof ]
                                    , paragraph [] [ italic "A", text " = ", text <| asPercentage rewardParameters.gasRewards.accountCreation ]
                                    , paragraph [] [ italic "U", text " = ", text <| asPercentage rewardParameters.gasRewards.chainUpdate ]
                                    ]
                                , paragraph [] [ text "The bakers fraction of the Gas Account is" ]
                                , paragraph [ Font.center ] [ text <| asPercentage rewardParameters.gasRewards.baker, text " + ", text <| asPercentage (1 - rewardParameters.gasRewards.baker), text " · ", italic "N" ]
                                ]
                                [ el [ centerX ] <| viewSpecialAccount ctx ctx.palette.fg1 "Gas account" event.oldGASAccount
                                , viewBar ctx <|
                                    [ { color = ctx.palette.c1, percentage = rewardParameters.gasRewards.baker, hint = "Baker Reward" } ]
                                        ++ (if nonGasFraction > 0 then
                                                [ { color = Palette.veryLight ctx.palette.c1, percentage = (1 - rewardParameters.gasRewards.baker) * nonGasFraction, hint = "Non-Gas\nBaker Reward" } ]

                                            else
                                                []
                                           )
                                        ++ [ { color = ctx.palette.c3, percentage = (1 - rewardParameters.gasRewards.baker) * (1 - nonGasFraction), hint = "Next Gas Account" } ]
                                ]
                            , viewDetailRow []
                                [ row [ spacing 20, centerX ]
                                    [ viewHeaderBox ctx ctx.palette.c1 "Baker Reward" <| viewPlusAmount ctx event.bakerReward
                                    , viewSpecialAccount ctx ctx.palette.c3 "Next Gas Account" event.newGASAccount
                                    , viewHeaderBox ctx ctx.palette.fg2 "Foundation" <| viewPlusAmount ctx event.foundationCharge
                                    ]
                                ]
                            ]
                    }
    in
    [ { content =
            viewItemRow ctx [] <|
                viewContentCells
                    { tipe = el [ stringTooltipAbove ctx item.tooltip ] (html <| item.icon)
                    , sender = el [ Font.color ctx.palette.fg1 ] <| text "Chain"
                    , event = item.content
                    , cost = none
                    , txHash = none
                    }
      , details = Just item.details
      }
    ]


italic : String -> Element msg
italic str =
    el [ Font.italic ] <| text str


super : String -> Element msg
super str =
    html <| Html.sup [] [ Html.text str ]


viewFinalizationData : Context a -> Maybe FinalizationData -> SummaryItem
viewFinalizationData ctx finalizationData =
    case finalizationData of
        Just data ->
            let
                totalWeight =
                    data.finalizers |> List.map .weight |> List.sum

                tableColumn attrs index value =
                    el
                        ([ padding 5, Font.center ]
                            ++ attrs
                            ++ (if isEven index then
                                    [ Background.color <| Palette.lightish ctx.palette.bg2 ]

                                else
                                    []
                               )
                        )
                    <|
                        Element.text value

                valueColumn index value =
                    tableColumn [] index value

                headerColumn value =
                    tableColumn [ Font.bold ] -1 value
            in
            [ { content =
                    viewItemRow ctx
                        []
                    <|
                        viewContentCells
                            { tipe = el [ stringTooltipAbove ctx "Finalization event", Font.color ctx.palette.c2 ] <| html <| Icons.block_finalized 20
                            , sender = el [ Font.color ctx.palette.fg1 ] <| text "Chain"
                            , event = row [] [ text <| "Finalized ", el [] <| text <| String.left 8 data.blockPointer ]
                            , cost = none
                            , txHash = none
                            }
              , details =
                    Just <|
                        column [ spacing 20 ]
                            [ viewDetailRow
                                [ paragraph [] [ text "A proof of a block being finalized." ]
                                , paragraph [] [ text <| "Finalized block: ", el [ onClick (BlockClicked data.blockPointer), pointer, Font.color ctx.palette.c2 ] <| text data.blockPointer ]
                                , paragraph [] [ text <| "Finalization index: " ++ String.fromInt data.index ]
                                , paragraph []
                                    [ text <|
                                        "The finalized block had to have at least "
                                            ++ String.fromInt data.delay
                                            ++ " descending block"
                                            ++ (if data.delay == 1 then
                                                    ""

                                                else
                                                    "s"
                                               )
                                            ++ ", when it was finalized."
                                    ]
                                ]
                                [ viewTable ctx
                                    { data = data.finalizers
                                    , columns =
                                        [ { header = text "Baker id"
                                          , width = fill
                                          , view =
                                                \i finalizer -> text <| String.fromInt finalizer.bakerId
                                          }
                                        , { header = text "Finalizer stake"
                                          , width = fill
                                          , view =
                                                \i finalizer -> text <| asPercentage (toFloat finalizer.weight / toFloat totalWeight)
                                          }
                                        , { header = text "Signed"
                                          , width = fill
                                          , view =
                                                \i finalizer ->
                                                    text <|
                                                        if finalizer.signed then
                                                            "yes"

                                                        else
                                                            "no"
                                          }
                                        ]
                                    }
                                ]
                            ]
              }
            ]

        Nothing ->
            []


viewTable : Context a -> { data : List records, columns : List (IndexedColumn records msg) } -> Element msg
viewTable ctx table =
    let
        tableColumn attrs index value =
            el
                ([ padding 5, Font.center ]
                    ++ attrs
                    ++ (if isEven index then
                            [ Background.color <| Palette.lightish ctx.palette.bg2 ]

                        else
                            []
                       )
                )
                value

        valueColumn index value =
            tableColumn [] index value

        headerColumn value =
            tableColumn [ Font.bold ] -1 value

        columns =
            List.map
                (\c ->
                    { header =
                        if c.header == Element.none then
                            Element.none

                        else
                            headerColumn c.header
                    , width = c.width
                    , view = \i d -> valueColumn i (c.view i d)
                    }
                )
                table.columns
    in
    Element.indexedTable
        [ Background.color ctx.palette.bg2
        , Border.width 1
        , Border.color ctx.palette.bg2
        , Border.rounded 5
        ]
        { data = table.data, columns = columns }


viewKeyValue : Context a -> List ( String, Element msg ) -> Element msg
viewKeyValue ctx data =
    viewTable ctx
        { data = data
        , columns =
            [ { header = Element.none
              , width = shrink
              , view = \i ( key, _ ) -> row [ width fill ] [ el [ Font.alignRight, Font.extraBold, width fill ] <| text key, text ":" ]
              }
            , { header = Element.none
              , width = fill
              , view = \i ( _, value ) -> el [ centerX ] value
              }
            ]
        }


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


viewTransactionEvent : Context a -> TransactionEvent -> TransactionEventItem
viewTransactionEvent ctx txEvent =
    case txEvent of
        -- Transfers
        TransactionEventTransferred event ->
            { content =
                [ row []
                    [ text <| "Transferred " ++ T.amountToString event.amount ++ " from "
                    , viewAddress ctx event.from
                    , text " to "
                    , viewAddress ctx event.to
                    ]
                ]
            , details = Nothing
            }

        TransactionEventTransferredWithSchedule event ->
            { content =
                [ row []
                    [ text <| "Transferred with schedule"
                    , arrowRight
                    , viewAddress ctx (T.AddressAccount event.to)
                    , text <| " " -- TODO: Use proper spacing
                    , viewTransferredWithScheduleDetails ctx event
                    ]
                ]
            , details = Nothing
            }

        TransactionEventEncryptedSelfAmountAdded event ->
            { content =
                [ row []
                    [ text <| T.amountToString event.amount ++ " was shielded on "
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
                ]
            , details = Nothing
            }

        TransactionEventAmountAddedByDecryption event ->
            { content =
                [ row []
                    [ text <| T.amountToString event.amount ++ " was unshielded on "
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
                ]
            , details = Nothing
            }

        -- Encrypted transfers
        TransactionEventNewEncryptedAmount event ->
            { content =
                [ row []
                    [ viewAddress ctx (T.AddressAccount event.account)
                    , text " received an encrypted amount."
                    ]
                ]
            , details = Nothing
            }

        TransactionEventEncryptedAmountsRemoved event ->
            { content =
                [ row []
                    [ viewAddress ctx (T.AddressAccount event.account)
                    , text " transferred an encrypted amount."
                    ]
                ]
            , details = Nothing
            }

        -- Accounts
        TransactionEventAccountCreated event ->
            { content =
                [ row
                    []
                    [ text <| "Created account"
                    , arrowRight
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
                ]
            , details = Nothing
            }

        TransactionEventCredentialDeployed event ->
            { content =
                [ row
                    []
                    [ text <| "Deployed credentials"
                    , arrowRight
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
                ]
            , details = Nothing
            }

        -- Account Keys
        TransactionEventAccountKeysUpdated ->
            -- TODO: Change icon
            { content =
                [ text "Updated account keys" ]
            , details = Nothing
            }

        TransactionEventAccountKeysAdded ->
            -- TODO: Change icon
            { content = [ text "Added account keys" ]
            , details = Nothing
            }

        TransactionEventAccountKeysRemoved ->
            -- TODO: Change icon
            { content = [ text "Removed account keys" ]
            , details = Nothing
            }

        TransactionEventAccountKeysSignThresholdUpdated ->
            -- TODO: Change icon
            { content = [ text "Updated signing threshold" ]
            , details = Nothing
            }

        -- Baking
        TransactionEventBakerAdded event ->
            { content =
                [ row []
                    [ text <| "Added"
                    , arrowRight
                    , viewBaker ctx event.bakerId event.account
                    ]
                ]
            , details = Nothing
            }

        TransactionEventBakerRemoved event ->
            { content =
                [ row []
                    [ text <| "Removed"
                    , arrowRight
                    , viewBaker ctx event.bakerId event.account
                    ]
                ]
            , details = Nothing
            }

        TransactionEventBakerStakeIncreased event ->
            { content =
                [ row []
                    [ text <| "Increased stake"
                    , arrowRight
                    , viewBaker ctx event.bakerId event.account
                    , arrowRight
                    , text <| T.amountToString event.newStake
                    ]
                ]
            , details = Nothing
            }

        TransactionEventBakerStakeDecreased event ->
            { content =
                [ row []
                    [ text <| "Decreased stake"
                    , arrowRight
                    , viewBaker ctx event.bakerId event.account
                    , arrowRight
                    , text <| T.amountToString event.newStake
                    ]
                ]
            , details = Nothing
            }

        TransactionEventBakerSetRestakeEarnings event ->
            { content =
                [ row []
                    [ text <| "Restake earnings"
                    , arrowRight
                    , viewBaker ctx event.bakerId event.account
                    , arrowRight
                    , text <|
                        if event.restakeEarnings then
                            "Set"

                        else
                            "Unset"
                    ]
                ]
            , details = Nothing
            }

        TransactionEventBakerKeysUpdated event ->
            { content =
                [ row []
                    [ text <| "Updated baker keys"
                    , arrowRight
                    , viewBaker ctx event.bakerId event.account
                    , arrowRight
                    , el
                        [ stringTooltipAboveWithCopy ctx event.signKey
                        , pointer
                        , onClick (CopyToClipboard event.signKey)
                        ]
                      <|
                        text <|
                            String.left 8 event.signKey
                    ]
                ]
            , details = Nothing
            }

        -- Contracts
        TransactionEventModuleDeployed event ->
            { content =
                [ row []
                    [ text <| "Deployed module"
                    , arrowRight
                    , el
                        [ stringTooltipAboveWithCopy ctx event.contents
                        , pointer
                        , onClick (CopyToClipboard event.contents)
                        ]
                      <|
                        text <|
                            String.left 8 event.contents
                    ]
                ]
            , details = Nothing
            }

        TransactionEventContractInitialized event ->
            { content =
                [ row []
                    [ text <| "Instantiated contract '" ++ event.contractName ++ "' with address: "
                    , viewAsAddressContract ctx event.address
                    , text <| " from module: " ++ String.left 8 event.ref
                    ]
                ]
            , details =
                Just <|
                    column [ width fill ]
                        [ viewDetailRow
                            [ paragraph [] [ text "Contract instance was initialized" ] ]
                            [ viewKeyValue ctx
                                [ ( "Module", el [ stringTooltipAboveWithCopy ctx "", pointer, onClick (CopyToClipboard event.ref) ] <| text event.ref )
                                , ( "Contract address", viewAddress ctx <| T.AddressContract event.address )
                                , ( "Contract", text <| event.contractName )
                                , ( "Amount", text <| T.amountToString event.amount )
                                ]
                            ]
                        , viewDetailRow
                            [ paragraph [] [ text "Contract events emitted" ] ]
                            [ if List.isEmpty event.events then
                                paragraph [ Font.center ] [ text "No events" ]

                              else
                                viewKeyValue ctx <| List.indexedMap (\i e -> ( String.fromInt i, text e )) event.events
                            ]
                        ]
            }

        TransactionEventContractUpdated event ->
            { content =
                [ row []
                    [ text <| "Updated contract instance at address: "
                    , viewAsAddressContract ctx event.address
                    ]
                ]
            , details =
                Just <|
                    column [ width fill ]
                        [ viewDetailRow
                            [ paragraph [] [ text "Contract instance was updated" ] ]
                            [ viewKeyValue ctx
                                [ ( "Contract address", viewAddress ctx <| T.AddressContract event.address )
                                , ( "Contract", text event.receiveName.contractName )
                                , ( "Function", text event.receiveName.functionName )
                                , ( "Amount", text <| T.amountToString event.amount )
                                ]
                            ]
                        , viewDetailRow
                            [ paragraph [] [ text "Contract events emitted" ] ]
                            [ if List.isEmpty event.events then
                                paragraph [ Font.center ] [ text "No events" ]

                              else
                                viewKeyValue ctx <| List.indexedMap (\i e -> ( String.fromInt i, text e )) event.events
                            ]
                        ]
            }

        TransactionEventUpdateEnqueued event ->
            { content =
                [ text <| "Update enqueued to take effect " ++ TimeHelpers.formatTime Time.utc event.effectiveTime
                ]
            , details =
                Just <|
                    viewEventUpdateEnueuedDetails ctx event
            }


viewEventUpdateEnueuedDetails : Context a -> EventUpdateEnqueued -> Element Msg
viewEventUpdateEnueuedDetails ctx event =
    case event.payload of
        MintDistributionPayload mintDistribution ->
            let
                foundationFraction =
                    1 - mintDistribution.bakingReward - mintDistribution.finalizationReward
            in
            viewDetailRow [ paragraph [] [ text "Updating the parameters for GTU minting." ] ]
                [ el [ centerX ] <| viewHeaderBox ctx ctx.palette.fg2 "Minted pr. slot" <| text <| String.fromFloat mintDistribution.mintPerSlot
                , viewBar
                    ctx
                    [ { color = ctx.palette.c1, percentage = mintDistribution.bakingReward, hint = bakingRewardAccountUpper }
                    , { color = ctx.palette.c2, percentage = mintDistribution.finalizationReward, hint = finalizationRewardAccountUpper }
                    , { color = ctx.palette.fg2, percentage = foundationFraction, hint = "Foundation" }
                    ]
                ]

        TransactionFeeDistributionPayload transactionFeeDistribution ->
            let
                foundationFraction =
                    1 - transactionFeeDistribution.baker - transactionFeeDistribution.gasAccount
            in
            viewDetailRow [ paragraph [] [ text "Updating the distribution of transaction fees." ] ]
                [ viewBar ctx
                    [ { color = ctx.palette.c1, percentage = transactionFeeDistribution.baker, hint = "Baker Reward" }
                    , { color = ctx.palette.c3, percentage = transactionFeeDistribution.gasAccount, hint = "Next Gas Account" }
                    , { color = ctx.palette.fg2, percentage = foundationFraction, hint = "Foundation" }
                    ]
                ]

        GasRewardsPayload gasRewards ->
            viewDetailRow
                [ paragraph [] [ text "Updating the parameters for the fraction of the Gas Account when baking blocks." ]
                , paragraph [] [ text "A block might include special transactions which do not cost a fee, these are account creation, chain updates and finalization proofs. Instead they are paid for by a fraction of the Gas Account." ]
                , paragraph [] [ text "let ", italic "N", text " be defined to be" ]
                , paragraph [ Font.center, Font.italic ] [ text "N = 1 - (1 - F)", super "f", text " · (1 - A)", super "a", text " · (1 - U)", super "u" ]
                , paragraph []
                    [ text "where "
                    , italic "f"
                    , text ", "
                    , italic "a"
                    , text " and "
                    , italic "u"
                    , text " are the number of finalization proofs, account creations and chain updates in this block respectively.\n"
                    , italic "F"
                    , text ", "
                    , italic "A"
                    , text " and "
                    , italic "U"
                    , text " are chain parameters."
                    ]
                ]
                [ row [ centerX, spacing 30 ]
                    [ paragraph [] [ italic "F", text " = ", text <| asPercentage gasRewards.finalizationProof ]
                    , paragraph [] [ italic "A", text " = ", text <| asPercentage gasRewards.accountCreation ]
                    , paragraph [] [ italic "U", text " = ", text <| asPercentage gasRewards.chainUpdate ]
                    ]
                , paragraph [ Font.center ] [ text "The bakers fraction of the Gas Account becomes" ]
                , paragraph [ Font.center ] [ text <| asPercentage gasRewards.baker, text " + ", text <| asPercentage (1 - gasRewards.baker), text " · ", italic "N" ]
                ]

        ElectionDifficultyPayload difficulty ->
            paragraph [ padding 20 ] [ text "Update the election difficulty to ", text <| asPercentage difficulty ]

        EuroPerEnergyPayload euroPerEnergy ->
            paragraph [ padding 20 ] [ text "Update the Euro per energy to ", text <| String.fromFloat euroPerEnergy ]

        MicroGtuPerEnergyPayload microGtuPerEnergy ->
            paragraph [ padding 20 ] [ text "Update the amount of μGTU per energy to ", text <| String.fromInt microGtuPerEnergy ]

        FoundationAccountPayload foundationAccount ->
            paragraph [ padding 20 ] [ text "Update the Foundation account to be ", viewAddress ctx <| T.AddressAccount foundationAccount ]

        AuthorizationPayload authorization ->
            paragraph [ padding 20 ] [ text "Update the chain update authorization." ]


viewAsAddressContract : Context a -> T.ContractAddress -> Element Msg
viewAsAddressContract ctx contractAddress =
    let
        content =
            "{\"index\":"
                ++ String.fromInt contractAddress.index
                ++ ",\"subindex\":"
                ++ String.fromInt contractAddress.subindex
                ++ "}"
    in
    el
        [ stringTooltipAboveWithCopy ctx content
        , pointer
        , onClick (CopyToClipboard content)
        ]
    <|
        viewAddress ctx <|
            T.AddressContract contractAddress


viewAddress : Context a -> T.Address -> Element Msg
viewAddress ctx addr =
    case addr of
        T.AddressAccount address ->
            row
                [ spacing 4
                , stringTooltipAboveWithCopy ctx address
                , pointer
                , onClick (CopyToClipboard address)
                ]
                [ el [] (html <| Icons.account_user 18)
                , text (String.left 8 address)
                ]

        T.AddressContract address ->
            row [ spacing 4 ]
                [ el [] (html <| Icons.smart_contract 18)
                , text <| T.contractAddressToString address
                ]


{-| View a baker as "<acc> (Baker: <baker-id>)". The account is shown using `viewAddress`.
-}
viewBaker : Context a -> Int -> T.AccountAddress -> Element Msg
viewBaker ctx bakerId addr =
    row
        [ spacing 4 ]
        [ viewAddress ctx <| T.AddressAccount addr
        , text <| "(Baker: " ++ String.fromInt bakerId ++ ")"
        ]


{-| Show 'details' and the release schedule on hover.
-}
viewTransferredWithScheduleDetails : Context a -> EventTransferredWithSchedule -> Element Msg
viewTransferredWithScheduleDetails ctx event =
    let
        viewRelease ( timestamp, amount ) =
            T.amountToString amount ++ " at " ++ TimeHelpers.formatTime Time.utc timestamp
    in
    viewDetailsTextWithOnHover ctx <|
        "Release schedule:\n\n"
            -- TODO: Show differently when list is large, as it can contain 255 releases.
            ++ (event.releaseSchedule |> List.map viewRelease |> String.join "\n")


{-| Show the text 'details' and, on hover, show the String provided.
-}
viewDetailsTextWithOnHover : Context a -> String -> Element Msg
viewDetailsTextWithOnHover ctx details =
    el
        [ Font.color (ctx.palette.c2 |> withAlphaEl 0.6)
        , stringTooltipAbove ctx details
        ]
    <|
        text "details"


{-| Show each account amount on a new line in the format: "<amount> → <acc>".
-}
showAccountAmounts : T.AccountAmounts -> String
showAccountAmounts accAmnt =
    let
        accountAmountToString ( accAddr, amount ) =
            T.amountToString amount ++ " → " ++ accAddr
    in
    accAmnt
        |> Dict.toList
        |> List.map accountAmountToString
        |> String.join "\n"
