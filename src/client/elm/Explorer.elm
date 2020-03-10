module Explorer exposing (..)

import Explorer.Request exposing (..)
import Http


type alias BlockHash =
    String


type alias Model =
    { currentBlockhash : Maybe String
    , currentBlockInfo : Maybe BlockInfo
    }


type Msg
    = ReceivedBlockInfo (Result Http.Error BlockInfo)
    | ReceivedConsensusStatus (Result Http.Error ConsensusStatus)
    | RequestedBlockInfo BlockHash


init =
    { currentBlockhash = Nothing
    , currentBlockInfo = Nothing
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
                    , Cmd.none
                    )

                Err err ->
                    let
                        x =
                            Debug.log <| "ReceivedBlockInfo:err" ++ httpErrorToString err
                    in
                    ( model, Cmd.none )

        RequestedBlockInfo blockHash ->
            ( model, getBlockInfo blockHash ReceivedBlockInfo )
