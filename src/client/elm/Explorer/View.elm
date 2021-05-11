module Explorer.View exposing (..)

import Api exposing (BlockInfo)
import Browser exposing (UrlRequest)
import Context exposing (Theme)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Explorer exposing (DisplayDetailBlockSummary, DisplayMsg(..), Model)
import Explorer.Request exposing (..)
import Html
import Html.Attributes exposing (style)
import Icons exposing (..)
import List
import Paging
import Palette exposing (withAlphaEl)
import Regex exposing (..)
import Set exposing (Set)
import String exposing (toLower)
import Svg exposing (Svg)
import Time
import TimeHelpers
import Tooltip exposing (..)
import Transaction.Event exposing (..)
import Transaction.Summary exposing (..)
import Types as T
import Widgets exposing (remoteDataView)


type Msg
    = CopyToClipboard String
    | BlockClicked String
    | Display DisplayMsg
    | UrlClicked UrlRequest
    | TransactionPaging Paging.Msg


type alias SummaryItem msg =
    List (SummaryItemEvent msg)


type alias SummaryItemEvent msg =
    { content : Element msg, details : Maybe (Element msg) }


mapSummaryItem : (a -> b) -> SummaryItem a -> SummaryItem b
mapSummaryItem fn item =
    List.map (mapSummaryItemEvent fn) item


mapSummaryItemEvent : (a -> b) -> SummaryItemEvent a -> SummaryItemEvent b
mapSummaryItemEvent fn x =
    { content = Element.map fn x.content
    , details = Maybe.map (Element.map fn) x.details
    }


bakingRewardAccountUpper =
    "Baking reward account"


bakingRewardAccountLower =
    toLower bakingRewardAccountUpper


finalizationRewardAccountUpper =
    "Finalization reward account"


finalizationRewardAccountLower =
    toLower finalizationRewardAccountUpper


view : Theme a -> Model -> Element Msg
view theme model =
    column [ spacing 40, width fill ]
        [ viewContainer theme
            (remoteDataView theme.palette
                (\blockInfo ->
                    column
                        [ width fill ]
                        [ viewHeader theme blockInfo
                        , remoteDataView theme.palette (viewBlockSummary theme) model.blockSummary
                        ]
                )
                model.blockInfo
            )
        ]


viewBlockSummary : Theme a -> DisplayDetailBlockSummary -> Element Msg
viewBlockSummary theme { blockSummary, state } =
    let
        transactionSummaries =
            blockSummary.transactionSummaries
                |> List.map (viewTransactionSummary theme)

        transactionNum = List.length transactionSummaries

        transactionPlural = if transactionNum == 1 then " transaction" else " transactions"

        transactionNumStr =  if List.isEmpty transactionSummaries
                             then ""
                             else " (" ++ (String.fromInt transactionNum) ++ transactionPlural ++ ")"

        transactionSummariesDescription =
            "Transactions included in this block" ++ transactionNumStr

        transactionPaging =
            Paging.paging state.transactionPagingModel transactionSummaries

        specialEvents =
            blockSummary.specialEvents
                |> List.map (viewSpecialEvent theme blockSummary.updates.chainParameters.rewardParameters)

        finalizations =
            viewFinalizationData theme blockSummary.finalizationData

        section =
            column [ width fill, padding 20 ]
    in
    column [ width fill ]
        [ section <|
            titleWithSubtitle theme "Transactions" transactionSummariesDescription
                :: (if List.isEmpty transactionSummaries then
                        [ column [ width fill, padding 20 ] [ el [ centerX, Font.color theme.palette.fg2 ] <| text "No transactions in this block." ] ]

                    else
                        [ row [ width fill, padding 10, spacing 15 ] <|
                            transactionRowCells
                                { tipe = none -- blockSummaryContentHeader theme "TYPE"
                                , sender = el [ centerX ] <| blockSummaryContentHeader theme "SENDER"
                                , event = blockSummaryContentHeader theme "EVENTS"
                                , cost = el [ centerX ] <| blockSummaryContentHeader theme "COST"
                                , txHash = blockSummaryContentHeader theme "TX HASH"
                                }
                        , column [ width fill, spacing 5 ] <| viewSummaryItems theme transactionPaging.visibleItems state.transactionWithDetailsOpen (\t e -> Display <| Explorer.ToggleTransactionDetails t e)
                        ]
                            ++ (if List.length transactionPaging.visibleItems < List.length transactionSummaries then
                                    [ el [ centerX, padding 15 ] <| Element.map TransactionPaging transactionPaging.pager ]

                                else
                                    []
                               )
                   )
        , section
            [ titleWithSubtitle theme "Tokenomics" "Distribution of transaction fees and minted tokens for this block"
            , row [ width fill, padding 10, spacing 15 ] <|
                transactionRowCells
                    { tipe = none
                    , sender = none
                    , event = blockSummaryContentHeader theme "EVENTS"
                    , cost = none
                    , txHash = none
                    }
            , column [ width fill, spacing 5 ] <| viewSummaryItem theme (List.concat specialEvents ++ finalizations) state.specialEventWithDetailsOpen (Display << ToggleSpecialEventDetails)
            ]
        , section
            [ titleWithSubtitle theme "Updates" "Updates queued at the time of this block"
            , viewUpdates theme blockSummary.updates
            ]
        ]


viewSummaryItems : Theme a -> List (SummaryItem msg) -> Dict Int (Set Int) -> (Int -> Int -> msg) -> List (Element msg)
viewSummaryItems theme summaryItems detailsDisplayed onEventClick =
    summaryItems
        |> List.indexedMap
            (\itemIndex summaryItem ->
                viewSummaryItem theme
                    summaryItem
                    (detailsDisplayed
                        |> Dict.get itemIndex
                        |> Maybe.withDefault Set.empty
                    )
                    (onEventClick itemIndex)
            )
        |> List.concat


viewSummaryItem : Theme a -> SummaryItem msg -> Set Int -> (Int -> msg) -> List (Element msg)
viewSummaryItem theme item itemDetailsDisplayed onEventClick =
    item
        |> List.indexedMap
            (\eventIndex summaryItemEvent ->
                viewSummaryItemEvent theme
                    summaryItemEvent
                    (Set.member eventIndex itemDetailsDisplayed)
                    (onEventClick eventIndex)
            )


viewSummaryItemEvent : Theme a -> SummaryItemEvent msg -> Bool -> msg -> Element msg
viewSummaryItemEvent theme event displayDetails onContentClick =
    case event.details of
        Just details ->
            column [ width fill ]
                [ el
                    ([ width fill
                     , onClick onContentClick
                     , pointer
                     ]
                        ++ buttonAttrs theme
                        ++ (if displayDetails then
                                [ Background.color theme.palette.bg3
                                , Border.roundEach { bottomLeft = 0, bottomRight = 0, topLeft = 10, topRight = 10 }
                                ]

                            else
                                []
                           )
                    )
                    event.content
                , el
                    ([ width fill
                     , Background.color <| Palette.veryLight theme.palette.bg2
                     ]
                        ++ collapsed (not displayDetails)
                    )
                    details
                ]

        Nothing ->
            event.content


viewContainer : Theme a -> Element msg -> Element msg
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


viewHeader : Theme a -> BlockInfo -> Element Msg
viewHeader theme blockInfo =
    row ([ width fill, spacing 15, paddingXY 6 0 ] ++ bottomBorder theme)
        [ viewParentLink theme blockInfo
        , viewBlockHash theme blockInfo.blockHash blockInfo.finalized
        , viewBlockStats theme blockInfo
        ]


viewParentLink : Theme a -> BlockInfo -> Element Msg
viewParentLink ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo.finalized
                |> withAlphaEl 0.5

        icon =
            blockIcon blockInfo.finalized 20
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


viewBlockHash : Theme a -> T.BlockHash -> Bool -> Element Msg
viewBlockHash theme blockHash finalized =
    let
        ( short, remaining ) =
            ( String.left 4 blockHash
            , String.dropLeft 4 blockHash
            )

        icon =
            blockIcon finalized 20

        copyIcon =
            Icons.copy_to_clipboard 18

        color =
            blockColor theme finalized
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
            , el [ stringTooltipAbove theme "Block hash" ]
                (paragraph []
                    [ el [ Font.color color ] (text short)
                    , text remaining
                    ]
                )
            , el [ width (px 10) ] none
            , el
                [ stringTooltipAbove theme "Copy to clipboard"
                , pointer
                , onClick (CopyToClipboard blockHash)
                ]
                (html copyIcon)
            ]
        )


blockColor : Theme a -> Bool -> Color
blockColor ctx finalized =
    if finalized then
        ctx.palette.c2

    else
        ctx.palette.c1


blockIcon : Bool -> Float -> Svg msg
blockIcon finalized =
    if finalized then
        Icons.block_finalized

    else
        Icons.block_not_finalized


viewBlockStats : Theme a -> BlockInfo -> Element Msg
viewBlockStats ctx blockInfo =
    row [ alignRight, spacing 15 ]
        [ viewBlockHeight ctx blockInfo
        , viewSlotTime ctx blockInfo
        ]


viewSlotTime : Theme a -> BlockInfo -> Element Msg
viewSlotTime ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo.finalized

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


viewBlockHeight : Theme a -> BlockInfo -> Element msg
viewBlockHeight ctx blockInfo =
    let
        color =
            blockColor ctx blockInfo.finalized
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


bottomBorder : Theme a -> List (Attribute msg)
bottomBorder ctx =
    [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
    , Border.color ctx.palette.bg1
    ]


transactionRowCells : { tipe : Element msg, sender : Element msg, event : Element msg, cost : Element msg, txHash : Element msg } -> List (Element msg)
transactionRowCells content =
    [ el [ width (shrink |> minimum 30) ] content.tipe
    , el [ width fill ] content.event
    , el [ width (shrink |> minimum 95) ] content.sender
    , el [ width (shrink |> minimum 120) ] content.cost
    , el [ width (shrink |> minimum 105) ] content.txHash
    ]


contentRowAttrs : List (Attribute msg)
contentRowAttrs =
    [ width fill
    , height (minimum 46 <| shrink)
    , paddingXY 10 0
    , spacing 15
    ]


buttonAttrs : Theme a -> List (Attribute msg)
buttonAttrs theme =
    [ Border.rounded 10
    , Border.width 2
    , Border.color theme.palette.bg3
    , Background.color <| Palette.lightish theme.palette.bg2
    , mouseOver [ Background.color theme.palette.bg3 ]
    ]


viewContentHeadline : Theme a -> Element msg
viewContentHeadline theme =
    row [ width fill, padding 10, spacing 15 ] <|
        transactionRowCells
            { tipe = none
            , sender = el [ centerX ] <| blockSummaryContentHeader theme "SENDER"
            , event = blockSummaryContentHeader theme "EVENTS"
            , cost = el [ centerX ] <| blockSummaryContentHeader theme "COST"
            , txHash = blockSummaryContentHeader theme "TX HASH"
            }


type alias TransactionEventItem msg =
    { content : List (Element msg)
    , details : Maybe (Element msg)
    }


viewTransactionSummary : Theme a -> TransactionSummary -> SummaryItem Msg
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
                    el [ Font.color ctx.palette.fg1 ] <| text "Governance"

                _ ->
                    none

        viewMainEventItem event =
            let
                item =
                    viewTransactionEvent ctx event
            in
            { content =
                row contentRowAttrs <|
                    transactionRowCells
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
                row contentRowAttrs <|
                    transactionRowCells
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
                    row contentRowAttrs <|
                        transactionRowCells
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
              , details = item.details
              }
            ]


type alias TypeDescription msg =
    { icon : Element msg

    {- |Short description

       It is used to display as the intention of the transaction, and is sometimes appended with ' failed:'.
    -}
    , short : String
    }


iconFromTypeDescription : Theme a -> TypeDescription msg -> Element msg
iconFromTypeDescription ctx tyDesc =
    el [ stringTooltipAbove ctx tyDesc.short ] tyDesc.icon


typeDescriptionTransactionSummaryType : TransactionSummaryType -> TypeDescription msg
typeDescriptionTransactionSummaryType transactionSummaryType =
    case transactionSummaryType of
        AccountTransaction ty ->
            typeDescriptionAccountTransactionType ty

        CredentialDeploymentTransaction ty ->
            typeDescriptionCredentialType ty

        UpdateTransaction ty ->
            typeDescriptionUpdateType ty


typeDescriptionAccountTransactionType : AccountTransactionType -> TypeDescription msg
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

        UpdateCredentialKeys ->
            { icon = html <| Icons.account_key_deployed 18, short = "Update credential keys" }

        EncryptedAmountTransfer ->
            { icon = html <| Icons.shield 20, short = "Shielded transfer" }

        TransferToEncrypted ->
            { icon = html <| Icons.shield 20, short = "Shield amount" }

        TransferToPublic ->
            { icon = html <| Icons.shield 20, short = "Unshield amount" }

        TransferWithSchedule ->
            { icon = html <| Icons.transaction 18, short = "Transfer with schedule" }

        UpdateCredentials ->
            { icon = html <| Icons.account_key_deployed 18, short = "Update account credentials" }

        RegisterData ->
            { icon = html <| Icons.smart_contract 20, short = "Register data" }

        Malformed ->
            { icon = el [ paddingXY 6 0 ] <| text "?", short = "Serialization" }


typeDescriptionCredentialType : CredentialType -> TypeDescription msg
typeDescriptionCredentialType credentialType =
    case credentialType of
        Initial ->
            { icon = html <| Icons.account_credentials_deployed 18, short = "Deploy initial credential" }

        Normal ->
            { icon = html <| Icons.account_credentials_deployed 18, short = "Deploy normal credential" }


typeDescriptionUpdateType : UpdateType -> TypeDescription msg
typeDescriptionUpdateType updateType =
    { icon = html <| Icons.system_cog 20
    , short = "Enqueue chain update"
    }


rejectionToItem : Theme a -> RejectReason -> { content : List (Element Msg), details : Maybe (Element Msg) }
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
            { content = let url = "https://developers.concordium.com/en/testnet4/testnet/references/manage-accounts.html#account-balances"
                        in [ text "The sending account ", viewAddress ctx <| account, text " has insufficient funds. Note: only funds that are not staked or locked can be transferred (see "
                           , link [ onClick <| UrlClicked <| Browser.External url ]
                                                           { url = url
                                                           , label = el [ Font.underline ] <| text "account balances"
                                                           }
                           , text " documentation)."
                           ]
            , details = Nothing
            }

        SerializationFailure ->
            { content = [ text "The transaction body was malformed" ]
            , details = Nothing
            }

        OutOfEnergy ->
            { content = [ text "The transaction ran out of energy" ]
            , details = Nothing
            }

        RejectedInit reject ->
            -- TODO: Extend with more information, such as the module reference and contract name
            { content = [ text <| "Contract refused to initialize with reason " ++ String.fromInt reject.rejectReason ]
            , details = Nothing
            }

        RejectedReceive reject ->
            -- TODO: Extend with more information, such as the contract name, method name and address
            { content = [ text <| "Rejected by contract logic with reason " ++ String.fromInt reject.rejectReason ]
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

        NonExistentCredentialID ->
            { content = [ text "Encountered credential ID that does not exist on the account" ]
            , details = Nothing
            }

        KeyIndexAlreadyInUse ->
            { content = [ text "The requested key index is already in use" ]
            , details = Nothing
            }

        InvalidAccountThreshold ->
            { content = [ text "The account threshold would exceed the number of credentials" ]
            , details = Nothing
            }

        InvalidCredentialKeySignThreshold ->
            { content = [ text "The signature threshold would exceed the number of keys of the credential" ]
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

        StakeUnderMinimumThresholdForBaking ->
            { content = [ text "The amount provided is under the threshold required for becoming a baker" ]
            , details = Nothing
            }

        BakerInCooldown ->
            { content = [ text "Request to make change to the baker while the baker is in the cooldown period" ]
            , details = Nothing
            }

        InvalidCredentials ->
            { content = [ text "One or more of the credentials is not valid" ]
            , details = Nothing
            }

        DuplicateCredIDs creds ->
            { content = [ text <| "Credential registration ids: " ++ String.join ", " creds ++ " are duplicate" ]
            , details = Nothing
            }

        NonExistentCredIDs creds ->
            { content = [ text <| "Credential registration ids: " ++ String.join ", " creds ++ " do not exist" ]
            , details = Nothing
            }

        RemoveFirstCredential ->
            { content = [ text "First credential of the account cannot be removed" ]
            , details = Nothing
            }

        CredentialHolderDidNotSign ->
            { content = [ text "Credential holder did not sign the credential key update" ]
            , details = Nothing
            }

        NotAllowedMultipleCredentials ->
            { content = [ text "Account is not allowed to have multiple credentials because it has non-zero encrypted balance" ]
            , details = Nothing
            }

        NotAllowedToReceiveEncrypted ->
            { content = [ text "Account is not allowed to receive encrypted transfers because it has multiple credentials" ]
            , details = Nothing
            }

        NotAllowedToHandleEncrypted ->
            { content = [ text "Account is not allowed to handle encrypted transfers because it has multiple credentials" ]
            , details = Nothing
            }


viewUpdates : Theme a -> Updates -> Element Msg
viewUpdates theme updates =
    let
        allQueuedUpdates =
            listUpdatePayloads updates.updateQueues
                |> List.sortBy (.effectiveTime >> Time.posixToMillis)

        updateRow left right =
            row [ width fill ] [ el [ width (fill |> maximum 250) ] left, el [ width fill ] right ]

        viewUpdate : EventUpdateEnqueued -> Element Msg
        viewUpdate update =
            updateRow
                (el
                    [ Font.color theme.palette.fg1 ]
                 <|
                    text <|
                        TimeHelpers.formatTime Time.utc <|
                            update.effectiveTime
                )
                (viewEventUpdateEnqueuedDetails theme update)
    in
    if List.isEmpty allQueuedUpdates then
        column [ width fill, padding 20 ] [ el [ centerX, Font.color theme.palette.fg2 ] <| text "No updates queued at the time of this block." ]

    else
        column [ width fill, padding 10, spacing 15 ]
            [ updateRow (blockSummaryContentHeader theme "EFFECTIVE TIME") (blockSummaryContentHeader theme "UPDATE")
            , column [ width fill, spacing 10 ] <| List.map viewUpdate allQueuedUpdates
            ]


blockSummaryContentHeader : Theme a -> String -> Element msg
blockSummaryContentHeader theme title =
    el [ Font.color theme.palette.fg2, Font.size 12 ] <| text title


titleWithSubtitle : Theme a -> String -> String -> Element msg
titleWithSubtitle theme title subtitle =
    el [ paddingEach { left = 0, right = 0, top = 0, bottom = 4 }, width fill ] <|
        row
            [ width fill
            , paddingEach { left = 0, right = 0, top = 0, bottom = 8 }
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Border.color theme.palette.fg3
            , spacing 15
            ]
            [ el
                [ Font.color theme.palette.fg1
                , Font.size 17
                ]
              <|
                text title
            , el [ Font.color theme.palette.fg2, Font.size 13 ] <| text subtitle
            ]


{-| Collect all the queued updates into one list
-}
listUpdatePayloads : UpdateQueues -> List EventUpdateEnqueued
listUpdatePayloads queues =
    let
        mapUpdate f q =
            List.map
                (\i ->
                    { effectiveTime = i.effectiveTime
                    , payload = f i.update
                    }
                )
                q.queue
    in
    mapUpdate RootKeysUpdatePayload queues.rootKeys
        ++ mapUpdate Level1KeysUpdatePayload queues.level1Keys
        ++ mapUpdate Level2KeysUpdatePayload queues.level2Keys
        ++ mapUpdate TransactionFeeDistributionPayload queues.transactionFeeDistribution
        ++ mapUpdate MicroGtuPerEuroPayload queues.microGTUPerEuro
        ++ mapUpdate ProtocolUpdatePayload queues.protocol
        ++ mapUpdate GasRewardsPayload queues.gasRewards
        ++ mapUpdate FoundationAccountPayload queues.foundationAccount
        ++ mapUpdate ElectionDifficultyPayload queues.electionDifficulty
        ++ mapUpdate EuroPerEnergyPayload queues.euroPerEnergy
        ++ mapUpdate MintDistributionPayload queues.mintDistribution
        ++ mapUpdate BakerStakeThresholdPayload queues.bakerStakeThreshold
        ++ mapUpdate AddAnonymityRevokerPayload queues.anonymityRevoker
        ++ mapUpdate AddIdentityProviderPayload queues.identityProvider


asPercentage : Float -> String
asPercentage n =
    String.left 5 (String.fromFloat (T.roundTo 4 n * 100)) ++ "%"


viewHeaderBox : Theme a -> Color -> String -> Element msg -> Element msg
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


viewSpecialAccount : Theme a -> Color -> String -> T.Amount -> Element Msg
viewSpecialAccount ctx color name amount =
    viewHeaderBox ctx color name <| text <| T.amountToString amount


viewPlusAmount : Theme a -> T.Amount -> Element Msg
viewPlusAmount ctx amount =
    el [ Font.color ctx.palette.success, Font.center, width fill ] <| text <| "+ " ++ T.amountToString amount


type alias BarPart =
    { color : Color
    , percentage : Float
    , hint : String
    }


viewBar : Theme a -> List BarPart -> Element msg
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


viewSpecialEvent : Theme a -> RewardParameters -> SpecialEvent -> SummaryItem Msg
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
                                T.sumAmounts <| List.map Tuple.second bakerRewardList

                            bakingAccountTotal =
                                T.addAmounts event.remainder bakingAccountDistributed
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
                                      , view = \i ( _, amount ) -> text <| asPercentage <| T.unsafeAmountDivide amount bakingAccountDistributed
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
                                T.sumAmounts [ event.mintPlatformDevelopmentCharge, event.mintBakingReward, event.mintFinalizationReward ]
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
                                T.sumAmounts <| List.map Tuple.second <| Dict.toList event.finalizationRewards

                            finalizationAccountTotal =
                                T.addAmounts event.remainder finalizationAccountDistributed
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
                                    , { header = text "Finalizer weight"
                                      , width = fill
                                      , view = \i ( _, amount ) -> text <| asPercentage <| T.unsafeAmountDivide amount finalizationAccountTotal
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
                            [ text <| "Rewarded " ++ T.amountToString event.bakerReward ++ " for baking this block to "
                            , viewAddress ctx (T.AddressAccount event.baker)
                            ]
                    , details =
                        let
                            foundationTransactionFeeBlockReward =
                                1 - (rewardParameters.transactionFeeDistribution.baker + rewardParameters.transactionFeeDistribution.gasAccount)

                            bakerRewardAmountTransactionFee =
                                T.scaleAmount rewardParameters.transactionFeeDistribution.baker event.transactionFees

                            bakerRewardAmountFixedGasAccount =
                                T.scaleAmount rewardParameters.gasRewards.baker event.oldGASAccount

                            nonGasBakerAmount =
                                T.subAmounts event.bakerReward <| T.addAmounts bakerRewardAmountTransactionFee bakerRewardAmountFixedGasAccount

                            nonGasFraction =
                                T.unsafeAmountDivide nonGasBakerAmount event.oldGASAccount
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
            row contentRowAttrs <|
                transactionRowCells
                    { tipe = el [ stringTooltipAbove ctx item.tooltip ] (html <| item.icon)
                    , sender = el [ Font.color ctx.palette.fg1 ] <| none --text "Chain"
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


capitalize : String -> String
capitalize str =
    case String.toList str of
        [] ->
            ""

        c :: cs ->
            String.fromList <| Char.toUpper c :: cs


viewFinalizationData : Theme a -> Maybe FinalizationData -> SummaryItem Msg
viewFinalizationData ctx finalizationData =
    case finalizationData of
        Just data ->
            let
                totalWeight =
                    data.finalizers |> List.map .weight |> List.sum
            in
            [ { content =
                    row contentRowAttrs <|
                        transactionRowCells
                            { tipe = el [ stringTooltipAbove ctx "Finalization event", Font.color ctx.palette.c2 ] <| html <| Icons.block_finalized 20
                            , sender = el [ Font.color ctx.palette.fg1 ] <| none --text "Chain"
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
                                        , { header = text "Finalizer weight"
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


viewTable : Theme a -> { data : List records, columns : List (IndexedColumn records msg) } -> Element msg
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
        , htmlAttribute <| style "overflow-y" "auto"
        , htmlAttribute <| style "max-height" "800px"
        ]
        { data = table.data, columns = columns }


viewKeyValue : Theme a -> List ( String, Element msg ) -> Element msg
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


wrapAttributes : List (Attribute msg)
wrapAttributes = width fill :: List.map htmlAttribute [ style "word-break" "break-word" ]


eventElem : List (Element msg) -> List (Element msg)
eventElem es = [ paragraph wrapAttributes es ]


viewTransactionEvent : Theme a -> TransactionEvent -> TransactionEventItem Msg
viewTransactionEvent ctx txEvent =
    case txEvent of
        -- Transfers
        TransactionEventTransferred event ->
            { content = eventElem
                    [ text <| "Transferred " ++ T.amountToString event.amount ++ " from "
                    , viewAddress ctx event.from
                    , text " to "
                    , viewAddress ctx event.to
                    ]
            , details = Nothing
            }

        TransactionEventTransferredWithSchedule event ->
            let
                totalAmount =
                    event.releaseSchedule
                        |> List.map Tuple.second
                        |> T.sumAmounts
            in
            { content = eventElem
                    [ text <| "Transferred with schedule to "
                    , viewAddress ctx (T.AddressAccount event.to)
                    ]
            , details =
                Just <|
                    column [ width fill ]
                        [ viewDetailRow
                            [ paragraph [] [ text <| capitalize <| T.amountToString totalAmount, text " was scheduled to be released." ] ]
                            [ viewTable ctx
                                { data = event.releaseSchedule
                                , columns =
                                    [ { header = text "Release date"
                                      , width = fill
                                      , view = \i ( timestamp, _ ) -> el [ centerX ] <| text <| TimeHelpers.formatTime Time.utc timestamp
                                      }
                                    , { header = text "Amount"
                                      , width = fill
                                      , view = \i ( _, amount ) -> text <| T.amountToString amount
                                      }
                                    ]
                                }
                            ]
                        ]
            }

        TransactionEventEncryptedSelfAmountAdded event ->
            { content = eventElem
                    [ text <| T.amountToString event.amount ++ " was shielded on "
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
            , details = Nothing
            }

        TransactionEventAmountAddedByDecryption event ->
            { content = eventElem
                    [ text <| T.amountToString event.amount ++ " was unshielded on "
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
            , details = Nothing
            }

        -- Encrypted transfers
        TransactionEventNewEncryptedAmount event ->
            { content = eventElem
                    [ viewAddress ctx (T.AddressAccount event.account)
                    , text " received an encrypted amount."
                    ]
            , details = Nothing
            }

        TransactionEventEncryptedAmountsRemoved event ->
            { content = eventElem
                    [ viewAddress ctx (T.AddressAccount event.account)
                    , text " transferred an encrypted amount."
                    ]
            , details = Nothing
            }

        -- Accounts
        TransactionEventAccountCreated event ->
            { content = eventElem
                    [ text <| "Created account "
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
            , details = Nothing
            }

        TransactionEventCredentialDeployed event ->
            { content = eventElem
                    [ text <| "Deployed credentials "
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
            , details = Nothing
            }

        -- Baking
        TransactionEventBakerAdded event ->
            { content = eventElem
                    [ text <| "Added baker "
                    , viewBaker ctx event.bakerId event.account
                    ]
            , details = Nothing
            }

        TransactionEventBakerRemoved event ->
            { content = eventElem
                    [ text <| "Removed baker "
                    , viewBaker ctx event.bakerId event.account
                    ]
            , details = Nothing
            }

        TransactionEventBakerStakeIncreased event ->
            { content = eventElem
                    [ text <| "Increased stake of "
                    , viewBaker ctx event.bakerId event.account
                    , text " to "
                    , text <| T.amountToString event.newStake
                    ]
            , details = Nothing
            }

        TransactionEventBakerStakeDecreased event ->
            { content = eventElem
                    [ text <| "Decreased stake of "
                    , viewBaker ctx event.bakerId event.account
                    , text " to "
                    , text <| T.amountToString event.newStake
                    ]
            , details = Nothing
            }

        TransactionEventBakerSetRestakeEarnings event ->
            { content = eventElem
                    [ text <|
                        if event.restakeEarnings then
                            "Enable"

                        else
                            "Disable"
                    , text <|
                        " restake earnings of "
                    , viewBaker ctx event.bakerId event.account
                    ]
            , details = Nothing
            }

        TransactionEventBakerKeysUpdated event ->
            { content = eventElem
                    [ text <| "Updated baker keys of "
                    , viewBaker ctx event.bakerId event.account
                    ]
            , details = Nothing
            }

        TransactionEventCredentialKeysUpdated event ->
            { content = eventElem
                    [ text <| "Updated keys and threshold of credential " ++ event.credId
                    ]
            , details = Nothing
            }

        TransactionEventCredentialsUpdated event ->
            { content = eventElem
                    [ text "Updated credentials of "
                    , viewAddress ctx (T.AddressAccount event.account)
                    ]
            , details =
                Just <|
                    column [ width fill ]
                        [ viewDetailRow
                            [ paragraph [] [ text "New credentials" ] ]
                            [ column [ spacing 5 ] <| List.map text event.newCredIds ]
                        , viewDetailRow
                            [ paragraph [] [ text "Removed credentials" ] ]
                            [ column [ spacing 5 ] <| List.map text event.removedCredIds ]
                        , viewDetailRow
                            [ paragraph [] [ text "New threshold" ] ]
                            [ text <| String.fromInt event.newThreshold ]
                        ]
            }

        -- Contracts
        TransactionEventModuleDeployed event ->
            { content = eventElem
                    [ text <| "Deployed module with reference "
                    , el
                        [ stringTooltipAboveWithCopy ctx event.contents
                        , pointer
                        , onClick (CopyToClipboard event.contents)
                        ]
                      <|
                        text <|
                            String.left 8 event.contents
                    ]
            , details = Nothing
            }

        TransactionEventContractInitialized event ->
            { content = eventElem
                    [ text <| "Instantiated contract '" ++ event.contractName ++ "' with address: "
                    , viewAsAddressContract ctx event.address
                    , text <| " from module: " ++ String.left 8 event.ref
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
            { content = eventElem
                    [ text <| "Updated contract instance at address: "
                    , viewAsAddressContract ctx event.address
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
            { content = eventElem
                [ text <| "Update enqueued to take effect " ++ TimeHelpers.formatTime Time.utc event.effectiveTime
                ]
            , details =
                Just <|
                    el [ padding 20 ] <|
                        viewEventUpdateEnqueuedDetails ctx event
            }

        TransactionEventDataRegistered event ->
            { content = eventElem
                [ text <| "Data registered on chain"
                ]
            , details =
                Just <|
                    column [ width fill ]
                        [ viewDetailRow
                            [ paragraph [] [ text "Hex representation of the registered data:" ] ]
                            [ paragraph [] [ text event.data ]
                            ]
                        ]
            }


viewEventUpdateEnqueuedDetails : Theme a -> EventUpdateEnqueued -> Element Msg
viewEventUpdateEnqueuedDetails ctx event =
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
            paragraph [] [ text "Update the election difficulty to ", text <| asPercentage difficulty ]

        EuroPerEnergyPayload euroPerEnergy ->
            row [ width fill ]
                [ text "Update the Euro per energy to "
                , viewRelation ctx euroPerEnergy
                ]

        MicroGtuPerEuroPayload microGtuPerEnergy ->
            row [ width fill ]
                [ text "Update the amount of μGTU per Euro to "
                , viewRelation ctx microGtuPerEnergy
                ]

        FoundationAccountPayload foundationAccount ->
            paragraph [] [ text "Update the Foundation account to be ", viewAddress ctx <| T.AddressAccount foundationAccount ]

        RootKeysUpdatePayload _ ->
            paragraph [] [ text "Update the chain-update root keys." ]

        Level1KeysUpdatePayload _ ->
            paragraph [] [ text "Update the chain-update level 1 keys." ]

        Level2KeysUpdatePayload _ ->
            paragraph [] [ text "Update the chain-update level 2 keys." ]

        ProtocolUpdatePayload protocolUpdate ->
            paragraph []
                [ text <| "Update the protocol: " ++ protocolUpdate.message ++ " "
                , link [ onClick <| UrlClicked <| Browser.External protocolUpdate.specificationURL ]
                    { url = protocolUpdate.specificationURL
                    , label = el [ Font.underline ] <| text "specification"
                    }
                ]

        BakerStakeThresholdPayload threshold ->
            paragraph [] [ text <| "Update the minimum staked amount for becoming a baker to " ++ T.amountToString threshold ]

        AddAnonymityRevokerPayload (ArInfo anonymityRevokerInfo) ->
            paragraph [] <| text ("Add a new anonymity revoker. ") :: displayArIp anonymityRevokerInfo

        AddIdentityProviderPayload (IpInfo identityProviderInfo) ->
            paragraph [] <| text ("Add a new identity provider. ") :: displayArIp identityProviderInfo


displayArIp : ArIpInfo -> List (Element Msg)
displayArIp info =
    let descr = info.description
    in displayName descr.name
    :: displayIdentity info.identity
    :: displayDescription descr.description
    :: displayWebsite descr.url


displayName: String -> Element Msg
displayName = displayStr "Name"


displayIdentity: Identity -> Element Msg
displayIdentity id = let i = case id of
                             ArIdentity ar -> ar
                             IpIdentity ip -> ip
                     in displayStr "Identity" <| String.fromInt i


displayDescription: String -> Element Msg
displayDescription = displayStr "Description"


{- Format a nonempty string with its corresponding attribute. This is used to display information on update
   transactions.
-}
displayStr: String -> String -> Element Msg
displayStr attrName str = let elem s = text <| attrName ++ ": " ++ s ++ ". "
                          in case Regex.fromString "[\\. ]*$" of -- to remove trailing spaces and periods
                               Nothing -> elem (String.trim str)
                               Just regex -> let trimmed = Regex.replace regex (\_ -> "") str
                                             in if trimmed == "" then text "" else elem trimmed


displayWebsite : String -> List (Element Msg)
displayWebsite url = if String.trim url == ""
                     then []
                     else [ text "Website: "
                          , link [ onClick <| UrlClicked <| Browser.External url ]
                                 { url = url
                                 , label = el [ Font.underline ] <| text url
                                 }
                          ]


{-| Display a relation as a fraction
-}
viewRelation : Theme a -> Relation -> Element msg
viewRelation ctx relation =
    column [ spacing 3 ]
        [ el [ centerX ] <| text <| String.fromInt relation.numerator
        , el [ width fill, Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }, Border.color ctx.palette.fg2 ] none
        , el [ centerX ] <| text <| String.fromInt relation.denominator
        ]


viewAsAddressContract : Theme a -> T.ContractAddress -> Element Msg
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


viewAddress : Theme a -> T.Address -> Element Msg
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
viewBaker : Theme a -> Int -> T.AccountAddress -> Element Msg
viewBaker ctx bakerId addr =
    row
        [ spacing 4 ]
        [ viewAddress ctx <| T.AddressAccount addr
        , text <| "(Baker: " ++ String.fromInt bakerId ++ ")"
        ]


{-| A list of attributes for animating a collapsible view
-}
collapsed : Bool -> List (Attribute msg)
collapsed isCollapsed =
    List.map htmlAttribute <|
        style "transition" "max-height 200ms ease-in"
            :: (if isCollapsed then
                    [ style "max-height" "0"
                    , style "overflow-y" "hidden"
                    ]

                else
                    [ style "max-height" "1000px"
                    ]
               )
