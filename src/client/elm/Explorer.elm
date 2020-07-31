module Explorer exposing (..)

import Explorer.Request exposing (..)
import Http
import RemoteData exposing (..)


type alias BlockHash =
    String


type alias Model =
    { config : Config
    , blockHash : Maybe String
    , blockInfo : WebData BlockInfo
    , blockSummary : WebData BlockSummary
    }


type Msg
    = ReceivedConsensusStatus (Result Http.Error ConsensusStatus)
    | ReceivedBlockInfo (Result Http.Error BlockInfo)
    | ReceivedBlockSummary (Result Http.Error BlockSummary)


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
                    ( model, getBlockInfo model.config consensusStatus.bestBlock ReceivedBlockInfo )

                Err err ->
                    ( model, Cmd.none )

        ReceivedBlockInfo blockInfoRes ->
            case blockInfoRes of
                Ok blockInfo ->
                    ( { model
                        | blockInfo =
                            Success blockInfo
                      }
                    , getBlockSummary model.config blockInfo.blockHash ReceivedBlockSummary
                    )

                Err err ->
                    ( model, Cmd.none )

        ReceivedBlockSummary blockSummaryResult ->
            ( { model
                | blockSummary =
                    RemoteData.fromResult blockSummaryResult
              }
            , Cmd.none
            )
