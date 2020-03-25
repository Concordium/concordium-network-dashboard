module Explorer exposing (..)

import Explorer.Request exposing (..)
import Http


type alias BlockHash =
    String


type alias Model =
    { currentBlockhash : Maybe String
    , currentBlockInfo : Maybe BlockInfo
    , currentBlockSummary : Maybe BlockSummary
    }


type Msg
    = ReceivedBlockInfo (Result Http.Error BlockInfo)
    | RequestedBlockInfo BlockHash
    | ReceivedConsensusStatus (Result Http.Error ConsensusStatus)
    | ReceivedBlockSummary (Result Http.Error BlockSummary)


init =
    { currentBlockhash = Nothing
    , currentBlockInfo = Nothing
    , currentBlockSummary = Nothing
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
                        | currentBlockInfo =
                            Just blockInfo
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

        ReceivedBlockSummary blockSummaryRes ->
            let
                y =
                    Debug.log "ReceivedBlockSummary" blockSummaryRes
            in
            case blockSummaryRes of
                Ok blockSummary ->
                    ( { model
                        | currentBlockSummary =
                            Just blockSummary
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        x =
                            Debug.log <| "ReceivedBlockSummary:err" ++ httpErrorToString err
                    in
                    ( model, Cmd.none )
