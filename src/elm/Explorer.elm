module Explorer exposing (..)

import Api exposing (ApiResult)
import Dict exposing (Dict)
import Explorer.Request exposing (..)
import Helpers exposing (toggleSetMember)
import Http exposing (Error(..))
import Paging
import RemoteData exposing (..)
import Set exposing (Set)
import Types as T


{-| Block summary together with state used when displaying.
-}
type alias DisplayDetailBlockSummary =
    { blockSummary : BlockSummary
    , state : BlockSummaryDisplayState
    }


{-| State used by the view when displaying the block summary
-}
type alias BlockSummaryDisplayState =
    { -- Used for paging of transactions and represents the index of the current page
      transactionPagingModel : Paging.Model

    -- Used for paging of transaction events.
    -- Mapping from an index of a transaction to the paging state for the events of that transaction.
    , transactionEventPagingModel : Dict Int Paging.Model

    -- A mapping from an index of a transaction to a set of indices of events
    -- in that transaction, with details currently open.
    , transactionWithDetailsOpen : Dict Int (Set Int)

    -- A set of indicies of events with details open.
    , specialEventWithDetailsOpen : Set Int
    }


initialBlockSummaryDisplayState : BlockSummaryDisplayState
initialBlockSummaryDisplayState =
    { transactionPagingModel = Paging.init 10
    , transactionEventPagingModel = Dict.empty
    , transactionWithDetailsOpen = Dict.empty
    , specialEventWithDetailsOpen = Set.empty
    }


{-| Initial state for paging of transaction events
-}
initialTransactionEventPaging =
    Paging.init 10


type alias Model =
    { config : Config
    , blockHash : Maybe T.BlockHash
    , blockInfo : WebData Api.BlockInfo
    , blockSummary : WebData DisplayDetailBlockSummary
    }


type Msg
    = ReceivedConsensusStatus (ApiResult Api.ConsensusStatus)
    | ReceivedBlockResponse (ApiResult Api.BlockResponse)
    | ReceivedBlockSummary (ApiResult BlockSummary)
    | Display DisplayMsg


{-| Messages for manipulating the display state
-}
type DisplayMsg
    = ToggleTransactionDetails Int Int
    | ToggleSpecialEventDetails Int
    | TransactionPaging Paging.Msg
    | TransactionEventPaging Int Paging.Msg


init : Config -> Model
init cfg =
    { config = cfg
    , blockHash = Nothing
    , blockInfo = NotAsked
    , blockSummary = NotAsked
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedConsensusStatus res ->
            case res of
                Ok consensusStatus ->
                    ( { model | blockInfo = Loading }
                    , Api.getBlockInfo model.config consensusStatus.bestBlock ReceivedBlockResponse
                    )

                Err err ->
                    ( model, Cmd.none )

        ReceivedBlockResponse blockInfoRes ->
            case blockInfoRes of
                Ok (Api.Block blockInfo) ->
                    ( { model
                        | blockInfo = Success blockInfo
                        , blockSummary = Loading
                      }
                    , getBlockSummary model.config blockInfo.blockHash ReceivedBlockSummary
                    )

                Ok (Api.BlockNotFound hash) ->
                    ( { model
                        | blockInfo = Failure <| BadBody <| "Block with hash '" ++ hash ++ "' does not exist on the chain."
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ReceivedBlockSummary blockSummaryResult ->
            ( { model
                | blockSummary =
                    blockSummaryResult
                        |> Result.map (\blockSummary -> { blockSummary = blockSummary, state = initialBlockSummaryDisplayState })
                        |> RemoteData.fromResult
              }
            , Cmd.none
            )

        Display displayMsg ->
            let
                nextBlockSummary =
                    model.blockSummary
                        |> RemoteData.map
                            (\displayDetailBlockSummary ->
                                { displayDetailBlockSummary
                                    | state = updateDisplayState displayMsg displayDetailBlockSummary.state
                                }
                            )
            in
            ( { model | blockSummary = nextBlockSummary }, Cmd.none )


updateDisplayState : DisplayMsg -> BlockSummaryDisplayState -> BlockSummaryDisplayState
updateDisplayState msg state =
    case msg of
        ToggleTransactionDetails transactionIndex eventIndex ->
            { state
                | transactionWithDetailsOpen =
                    Dict.update
                        transactionIndex
                        (Maybe.withDefault Set.empty >> toggleSetMember eventIndex >> Just)
                        state.transactionWithDetailsOpen
            }

        ToggleSpecialEventDetails eventIndex ->
            { state
                | specialEventWithDetailsOpen = toggleSetMember eventIndex state.specialEventWithDetailsOpen
            }

        TransactionPaging pagingMsg ->
            { state
                | transactionPagingModel = Paging.update pagingMsg state.transactionPagingModel
                , transactionWithDetailsOpen = Dict.empty
            }

        TransactionEventPaging txIndex pagingMsg ->
            { state
                | transactionEventPagingModel =
                    state.transactionEventPagingModel
                        |> Dict.update txIndex
                            (\pagingModel ->
                                pagingModel
                                    |> Maybe.withDefault initialTransactionEventPaging
                                    |> Paging.update pagingMsg
                                    |> Just
                            )
            }
