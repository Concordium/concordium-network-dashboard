module Explorer exposing (..)

import Chain exposing (Msg(..))
import Config exposing (Config)
import Context exposing (Context)
import Explorer.Request exposing (..)
import Http
import RemoteData exposing (..)


type alias BlockHash =
    String


type alias Model =
    { config : Config
    , chainModel : Chain.Model
    , blockHash : Maybe String
    , blockInfo : WebData BlockInfo
    , blockSummary : WebData BlockSummary
    }


type Msg
    = ReceivedConsensusStatus (Result Http.Error ConsensusStatus)
    | ReceivedBlockInfo (Result Http.Error BlockInfo)
    | ReceivedBlockSummary (Result Http.Error BlockSummary)
    | ChainMsg Chain.Msg


init : Config -> ( Model, Cmd Msg )
init cfg =
    let
        ( chainModel, chainCmd ) =
            Chain.init cfg.collectorUrl
    in
    ( { config = cfg
      , blockHash = Nothing
      , blockInfo = NotAsked
      , blockSummary = NotAsked
      , chainModel = chainModel
      }
    , Cmd.map ChainMsg chainCmd
    )


update : Context a -> Msg -> Model -> ( Model, Cmd (Context.Msg Msg) )
update ctx msg model =
    case msg of
        ReceivedConsensusStatus res ->
            case res of
                Ok consensusStatus ->
                    ( model, getBlockInfo model.config consensusStatus.bestBlock (Context.Local << ReceivedBlockInfo) )

                Err err ->
                    ( model, Cmd.none )

        ReceivedBlockInfo blockInfoRes ->
            case blockInfoRes of
                Ok blockInfo ->
                    ( { model
                        | blockInfo =
                            Success blockInfo
                      }
                    , getBlockSummary model.config blockInfo.blockHash (Context.Local << ReceivedBlockSummary)
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

        ChainMsg chainMsg ->
            let
                ( chainModel, chainCmd ) =
                    Chain.update ctx chainMsg model.chainModel
            in
            ( { model | chainModel = chainModel }, Cmd.map (Context.Local << ChainMsg) chainCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ChainMsg <| Chain.subscriptions model.chainModel
