module Explorer exposing (..)

import Explorer.Request exposing (..)
import Http
import RemoteData exposing (..)
import Set exposing (Set)


type alias BlockHash =
    String


type alias DisplayDetailBlockSummary =
    { blockSummary : BlockSummary

    -- A set of indexes of transactions with details actively displayed in the view.
    , detailsDisplayed : Set Int
    }


type alias Model =
    { config : Config
    , blockHash : Maybe String
    , blockInfo : WebData BlockInfo
    , blockSummary : WebData DisplayDetailBlockSummary
    }


type Msg
    = ReceivedConsensusStatus (Result Http.Error ConsensusStatus)
    | ReceivedBlockInfo (Result Http.Error BlockInfo)
    | ReceivedBlockSummary (Result Http.Error BlockSummary)
    | ToggleDisplayDetails Int


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
                    , getBlockInfo model.config consensusStatus.bestBlock ReceivedBlockInfo
                    )

                Err err ->
                    ( model, Cmd.none )

        ReceivedBlockInfo blockInfoRes ->
            case blockInfoRes of
                Ok blockInfo ->
                    ( { model
                        | blockInfo = Success blockInfo
                        , blockSummary = Loading
                      }
                    , getBlockSummary model.config blockInfo.blockHash ReceivedBlockSummary
                    )

                Err err ->
                    ( model, Cmd.none )

        ReceivedBlockSummary blockSummaryResult ->
            ( { model
                | blockSummary =
                    blockSummaryResult
                        |> Result.map (\blockSummary -> { blockSummary = blockSummary, detailsDisplayed = Set.empty })
                        |> RemoteData.fromResult
              }
            , Cmd.none
            )

        ToggleDisplayDetails index ->
            let
                nextBlockSummary =
                    model.blockSummary
                        |> RemoteData.map
                            (\displayDetailBlockSummary ->
                                { displayDetailBlockSummary
                                    | detailsDisplayed =
                                        let
                                            detailsDisplayed =
                                                displayDetailBlockSummary.detailsDisplayed
                                        in
                                        if Set.member index detailsDisplayed then
                                            Set.remove index detailsDisplayed

                                        else
                                            Set.insert index detailsDisplayed
                                }
                            )
            in
            ( { model | blockSummary = nextBlockSummary }, Cmd.none )
