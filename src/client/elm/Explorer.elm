module Explorer exposing (..)

import Dict exposing (Dict)
import Explorer.Request exposing (..)
import Http
import RemoteData exposing (..)
import Set exposing (Set)


type alias BlockHash =
    String


type alias DisplayDetailBlockSummary =
    { blockSummary : BlockSummary

    -- A set of indexes of transactions with details actively displayed in the view.
    , detailsDisplayed : Dict Int (Set Int)
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
    | ToggleDisplayDetails Int Int


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
                        |> Result.map (\blockSummary -> { blockSummary = blockSummary, detailsDisplayed = Dict.empty })
                        |> RemoteData.fromResult
              }
            , Cmd.none
            )

        ToggleDisplayDetails itemIndex eventIndex ->
            let
                nextBlockSummary =
                    model.blockSummary
                        |> RemoteData.map
                            (\displayDetailBlockSummary ->
                                { displayDetailBlockSummary
                                    | detailsDisplayed =
                                        let
                                            eventSet =
                                                displayDetailBlockSummary.detailsDisplayed
                                                    |> Dict.get itemIndex
                                                    |> Maybe.withDefault Set.empty

                                            newEventSet =
                                                if Set.member eventIndex eventSet then
                                                    Set.remove eventIndex eventSet

                                                else
                                                    Set.insert eventIndex eventSet
                                        in
                                        Dict.insert itemIndex newEventSet displayDetailBlockSummary.detailsDisplayed
                                }
                            )
            in
            ( { model | blockSummary = nextBlockSummary }, Cmd.none )
