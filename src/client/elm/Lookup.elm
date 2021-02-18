module Lookup exposing (..)

import Api exposing (ApiResult)
import Browser.Navigation as Nav
import Context exposing (Theme)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Explorer.View exposing (mapSummaryItem, viewBlockHash, viewContainer, viewContentHeadline, viewSummaryItem, viewTransactionSummary)
import Helpers exposing (..)
import List
import RemoteData exposing (WebData)
import Route exposing (Route)
import Set exposing (Set)
import Types as T
import Widgets


type alias DisplayDetailTransactionStatus =
    { transactionStatus : Api.TransactionStatus

    -- A map from index of block to a set of event indexes with details actively displayed in the view.
    , detailsDisplayed : Dict Int (Set Int)
    }


type alias Model =
    { navigationKey : Nav.Key
    , searchTextValue : String
    , transactionStatusResult : WebData DisplayDetailTransactionStatus
    }


init : Nav.Key -> Model
init navigationKey =
    { navigationKey = navigationKey
    , searchTextValue = ""
    , transactionStatusResult = RemoteData.NotAsked
    }


type Msg
    = SetSearchTextValue String
    | SearchForTransaction T.TxHash
    | ReceivedTransactionStatus (ApiResult Api.TransactionStatus)
    | CopyToClipboard String -- Is mapped and handled in Main module
    | ToggleDisplayDetails Int Int
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSearchTextValue txt ->
            ( { model | searchTextValue = txt }, Cmd.none )

        SearchForTransaction txHash ->
            ( { model | transactionStatusResult = RemoteData.Loading }
            , Nav.pushUrl model.navigationKey <| Route.toString <| Route.LookupTransaction txHash
            )

        ReceivedTransactionStatus result ->
            ( { model
                | transactionStatusResult =
                    RemoteData.fromResult result
                        |> RemoteData.map
                            (\txStatus -> { transactionStatus = txStatus, detailsDisplayed = Dict.empty })
              }
            , Cmd.none
            )

        ToggleDisplayDetails itemIndex eventIndex ->
            let
                nextTransactionStatusResult =
                    model.transactionStatusResult
                        |> RemoteData.map
                            (\displayDetailTransactionStatus ->
                                { displayDetailTransactionStatus
                                    | detailsDisplayed =
                                        Dict.update itemIndex
                                            (Maybe.withDefault Set.empty >> toggleSetMember eventIndex >> Just)
                                            displayDetailTransactionStatus.detailsDisplayed
                                }
                            )
            in
            ( { model | transactionStatusResult = nextTransactionStatusResult }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Theme a -> Model -> Element Msg
view theme model =
    column
        [ spacing 40
        , width fill
        ]
        [ viewTransactionSearch theme model
        , viewTransactionStatusWebData theme model.transactionStatusResult
        ]


viewTransactionSearch : Theme a -> Model -> Element Msg
viewTransactionSearch theme model =
    column
        [ centerX
        , spacing 10
        , width
            (fill
                |> maximum 580
            )
        ]
        [ Input.text
            ([ Background.color theme.palette.bg1
             , Border.color theme.palette.fg2
             , Border.rounded 8
             , onEnter <| SearchForTransaction model.searchTextValue
             ]
                ++ (if String.isEmpty model.searchTextValue then
                        [ Input.focusedOnLoad ]

                    else
                        []
                   )
            )
            { onChange = SetSearchTextValue
            , text = model.searchTextValue
            , label = Input.labelAbove [ Font.center, paddingXY 5 10, Font.size 20 ] <| text "Lookup a transaction"
            , placeholder = Just <| Input.placeholder [ Font.color theme.palette.fg2 ] <| text "Transaction hash"
            }
        , Input.button
            [ centerX
            , Font.center
            , paddingXY 20 10
            , Border.rounded 5
            , Background.color theme.palette.fg3
            , focused
                [ Background.color theme.palette.bg3
                ]
            ]
            { onPress = Just <| SearchForTransaction model.searchTextValue
            , label = el [ Font.color theme.palette.fg1 ] <| text "Lookup"
            }
        ]


viewTransactionStatusWebData : Theme a -> WebData DisplayDetailTransactionStatus -> Element Msg
viewTransactionStatusWebData theme remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            Element.none

        RemoteData.Loading ->
            Widgets.loader theme.palette.fg3

        RemoteData.Failure error ->
            el [ Font.color theme.palette.danger ] (paragraph [] [ text <| "Error: " ++ Widgets.errorToString error ])

        RemoteData.Success data ->
            viewTransactionStatus theme data


viewTransactionStatus : Theme a -> DisplayDetailTransactionStatus -> Element Msg
viewTransactionStatus theme data =
    let
        viewTransactionStatusBlock finalized index ( blockHash, txSummary ) =
            let
                item =
                    mapSummaryItem explorerToLookupMsg <| viewTransactionSummary theme txSummary

                displayedEvents =
                    data.detailsDisplayed
                        |> Dict.get index
                        |> Maybe.withDefault Set.empty
            in
            viewContainer theme <|
                column [ centerX, spacing 5, width fill ] <|
                    [ el [ centerX ] (Element.map explorerToLookupMsg <| viewBlockHash theme blockHash finalized)
                    , viewContentHeadline theme
                    , column [ width fill ] <| viewSummaryItem theme item displayedEvents (ToggleDisplayDetails index)
                    ]
    in
    case data.transactionStatus of
        Api.Received ->
            el [ centerX, padding 10 ] <| text "Transaction is received by the node, but not committed to a block yet."

        Api.Committed blocks ->
            let
                numberOfBlocks =
                    Dict.size blocks

                inBlockMessage =
                    if numberOfBlocks == 1 then
                        "The transaction is not finalized yet, but committed to the block:"

                    else
                        "The transaction is not finalized yet, but committed to " ++ String.fromInt numberOfBlocks ++ " blocks:"
            in
            column [ spacing 20, width fill ] <|
                (el [ centerX, padding 10 ] <| text inBlockMessage)
                    :: List.indexedMap (viewTransactionStatusBlock False) (Dict.toList blocks)

        Api.Finalized block ->
            column [ spacing 20, width fill ]
                [ el [ centerX, padding 10 ] <| text "The transaction was finalized in block:", viewTransactionStatusBlock True 0 block ]


explorerToLookupMsg : Explorer.View.Msg -> Msg
explorerToLookupMsg emsg =
    case emsg of
        Explorer.View.CopyToClipboard str ->
            CopyToClipboard str

        _ ->
            None
