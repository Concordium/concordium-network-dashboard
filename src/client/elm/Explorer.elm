module Explorer exposing (..)

import Explorer.Request exposing (..)
import Http
import RemoteData exposing (..)


type alias BlockHash =
    String


type alias Model =
    { blockHash : Maybe String
    , blockInfo : WebData BlockInfo
    , blockSummary : WebData BlockSummary
    }


type Msg
    = ReceivedBlockInfo (Result Http.Error BlockInfo)
    | RequestedBlockInfo BlockHash
    | ReceivedConsensusStatus (Result Http.Error ConsensusStatus)
    | ReceivedBlockSummary (Result Http.Error BlockSummary)


init : Model
init =
    { blockHash = Nothing

    -- , blockInfo = Success blockInfoStub
    , blockInfo = NotAsked

    -- , blockSummary = Success getBlockSummaryStub_
    , blockSummary = NotAsked
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedConsensusStatus res ->
            case res of
                Ok consensusStatus ->
                    ( model, getBlockInfo consensusStatus.bestBlock ReceivedBlockInfo )

                Err err ->
                    let
                        x =
                            Debug.log <| "ReceivedConsensusStatus:err" ++ httpErrorToString err
                    in
                    ( model, Cmd.none )

        RequestedBlockInfo blockHash ->
            ( model, getBlockInfo blockHash ReceivedBlockInfo )

        ReceivedBlockInfo blockInfoRes ->
            let
                y =
                    Debug.log "ReceivedBlockInfo" blockInfoRes
            in
            case blockInfoRes of
                Ok blockInfo ->
                    ( { model
                        | blockInfo =
                            Success blockInfo
                      }
                    , if blockInfo.transactionCount > 0 then
                        getBlockSummary blockInfo.blockHash ReceivedBlockSummary

                      else
                        Cmd.none
                    )

                Err err ->
                    let
                        x =
                            Debug.log <| "ReceivedBlockInfo:err" ++ httpErrorToString err
                    in
                    ( model, Cmd.none )

        ReceivedBlockSummary blockSummaryResult ->
            let
                y =
                    Debug.log "ReceivedBlockSummary" blockSummaryResult
            in
            ( { model
                | blockSummary =
                    RemoteData.fromResult blockSummaryResult
              }
            , Cmd.none
            )
