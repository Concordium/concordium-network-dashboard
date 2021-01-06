module Explorer.Request exposing (..)

import Http exposing (..)
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Task
import Time exposing (Posix)
import Transaction.Event exposing (GasRewards, MintDistribution, TransactionFeeDistribution, gasRewardsDecoder, mintDistributionDecoder, transactionFeeDistributionDecoder)
import Transaction.Summary exposing (..)
import Types as T


type alias Config =
    { middlewareUrl : String
    }



-- ConsensusStatus


type alias ConsensusStatus =
    { bestBlock : String
    , genesisTime : Posix
    , epochDuration : Int
    }


consensusStatusDecoder : D.Decoder ConsensusStatus
consensusStatusDecoder =
    D.succeed ConsensusStatus
        |> required "bestBlock" D.string
        |> required "genesisTime" Iso8601.decoder
        |> required "epochDuration" D.int


getConsensusStatus : Config -> (Result Http.Error ConsensusStatus -> msg) -> Cmd msg
getConsensusStatus cfg msg =
    Http.get
        { url = cfg.middlewareUrl ++ "/v1/consensusStatus"
        , expect = expectJson_ msg consensusStatusDecoder
        }



-- BlockInfo


getBlockInfo : Config -> String -> (Result Error BlockInfo -> msg) -> Cmd msg
getBlockInfo cfg blockhash msg =
    Http.get
        { url = cfg.middlewareUrl ++ "/v1/blockInfo/" ++ blockhash
        , expect = expectJson_ msg blockInfoDecoder
        }



-- BlockSummary


getBlockSummary : Config -> String -> (Result Error BlockSummary -> msg) -> Cmd msg
getBlockSummary cfg blockhash msg =
    -- let
    --     x =
    --         Debug.log "calling" "get Block summary!"
    -- in
    Http.get
        { url = cfg.middlewareUrl ++ "/v1/blockSummary/" ++ blockhash
        , expect = expectJson_ msg blockSummaryDecoder
        }


type alias BlockInfo =
    { transactionsSize : Int
    , blockParent : String
    , blockHash : String
    , finalized : Bool
    , blockArriveTime : Posix
    , blockReceiveTime : Posix
    , transactionCount : Int
    , transactionEnergyCost : Int
    , blockSlot : Int
    , blockLastFinalized : String
    , blockSlotTime : Posix
    , blockHeight : Int
    , blockBaker : Int
    }


blockInfoDecoder : D.Decoder BlockInfo
blockInfoDecoder =
    D.succeed BlockInfo
        |> required "transactionsSize" D.int
        |> required "blockParent" D.string
        |> required "blockHash" D.string
        |> required "finalized" D.bool
        |> required "blockArriveTime" Iso8601.decoder
        |> required "blockReceiveTime" Iso8601.decoder
        |> required "transactionCount" D.int
        |> required "transactionEnergyCost" D.int
        |> required "blockSlot" D.int
        |> required "blockLastFinalized" D.string
        |> required "blockSlotTime" Iso8601.decoder
        |> required "blockHeight" D.int
        |> required "blockBaker" D.int


trigger : msg -> Cmd msg
trigger msg =
    Task.perform identity (Task.succeed msg)


type alias BlockSummary =
    { specialEvents : List SpecialEvent
    , transactionSummaries : List TransactionSummary
    , finalizationData : Maybe FinalizationData
    , updates : Updates
    }


blockSummaryDecoder : D.Decoder BlockSummary
blockSummaryDecoder =
    D.succeed BlockSummary
        |> required "specialEvents" (D.list specialEventDecoder)
        |> required "transactionSummaries" (D.list transactionSummaryDecoder)
        |> optional "finalizationData" (D.nullable finalizationDataDecoder) Nothing
        |> required "updates" updatesDecoder



-- Updates


type alias Updates =
    { chainParameters : ChainParameters
    }


type alias ChainParameters =
    { rewardParameters : RewardParameters
    , microGTUPerEuro : Relation
    , foundationAccountIndex : Int
    , accountCreationLimit : Int
    , bakerCooldownEpochs : Int
    , electionDifficulty : Float
    , euroPerEnergy : Relation
    }


type alias RewardParameters =
    { mintDistribution : MintDistribution
    , transactionFeeDistribution : TransactionFeeDistribution
    , gasRewards : GasRewards
    }


type alias Relation =
    { denominator : Int
    , numerator : Int
    }


updatesDecoder : D.Decoder Updates
updatesDecoder =
    D.succeed Updates
        |> required "chainParameters" chainParametersDecoder


chainParametersDecoder : D.Decoder ChainParameters
chainParametersDecoder =
    D.succeed ChainParameters
        |> required "rewardParameters" rewardParametersDecoder
        |> required "microGTUPerEuro" relationDecoder
        |> required "foundationAccountIndex" D.int
        |> required "accountCreationLimit" D.int
        |> required "bakerCooldownEpochs" D.int
        |> required "electionDifficulty" D.float
        |> required "euroPerEnergy" relationDecoder


rewardParametersDecoder : D.Decoder RewardParameters
rewardParametersDecoder =
    D.succeed RewardParameters
        |> required "mintDistribution" mintDistributionDecoder
        |> required "transactionFeeDistribution" transactionFeeDistributionDecoder
        |> required "gASRewards" gasRewardsDecoder


relationDecoder : D.Decoder Relation
relationDecoder =
    D.succeed Relation
        |> required "denominator" D.int
        |> required "numerator" D.int



-- SpecialEvents


type SpecialEvent
    = SpecialEventBakingRewards BakingRewards
    | SpecialEventMint Mint
    | SpecialEventFinalizationRewards FinalizationRewards
    | SpecialEventBlockReward BlockReward


type alias BakingRewards =
    { bakerRewards : T.AccountAmounts
    , remainder : T.Amount
    }


type alias Mint =
    { mintBakingReward : T.Amount
    , mintFinalizationReward : T.Amount
    , mintPlatformDevelopmentCharge : T.Amount
    , foundationAccount : T.AccountAddress
    }


type alias FinalizationRewards =
    { finalizationRewards : T.AccountAmounts
    , remainder : T.Amount
    }


type alias BlockReward =
    { transactionFees : T.Amount
    , oldGASAccount : T.Amount
    , newGASAccount : T.Amount
    , bakerReward : T.Amount
    , foundationCharge : T.Amount
    , foundationAccount : T.AccountAddress
    , baker : T.AccountAddress
    }


specialEventDecoder : D.Decoder SpecialEvent
specialEventDecoder =
    let
        decode : String -> D.Decoder SpecialEvent
        decode tag =
            case tag of
                "BakingRewards" ->
                    D.succeed BakingRewards
                        |> required "bakerRewards" T.accountAmountsDecoder
                        |> required "remainder" T.decodeAmount
                        |> D.map SpecialEventBakingRewards

                "Mint" ->
                    D.succeed Mint
                        |> required "mintBakingReward" T.decodeAmount
                        |> required "mintFinalizationReward" T.decodeAmount
                        |> required "mintPlatformDevelopmentCharge" T.decodeAmount
                        |> required "foundationAccount" T.accountAddressDecoder
                        |> D.map SpecialEventMint

                "FinalizationRewards" ->
                    D.succeed FinalizationRewards
                        |> required "finalizationRewards" T.accountAmountsDecoder
                        |> required "remainder" T.decodeAmount
                        |> D.map SpecialEventFinalizationRewards

                "BlockReward" ->
                    D.succeed BlockReward
                        |> required "transactionFees" T.decodeAmount
                        |> required "oldGASAccount" T.decodeAmount
                        |> required "newGASAccount" T.decodeAmount
                        |> required "bakerReward" T.decodeAmount
                        |> required "foundationCharge" T.decodeAmount
                        |> required "foundationAccount" T.accountAddressDecoder
                        |> required "baker" T.accountAddressDecoder
                        |> D.map SpecialEventBlockReward

                _ ->
                    D.fail """Invalid SpecialEvent tag.
                              Expected one of the following: BakingRewards, Mint,
                              FinalizationRewards, BlockReward."""
    in
    D.field "tag" D.string |> D.andThen decode


type alias FinalizationData =
    { blockPointer : String
    , index : Int
    , delay : Int
    , finalizers : List FinalizerInfo
    }


finalizationDataDecoder : D.Decoder FinalizationData
finalizationDataDecoder =
    D.succeed FinalizationData
        |> required "finalizationBlockPointer" D.string
        |> required "finalizationIndex" D.int
        |> required "finalizationDelay" D.int
        |> required "finalizers" (D.list finalizerInfoDecoder)


type alias FinalizerInfo =
    { bakerId : Int
    , weight : Int
    , signed : Bool
    }


finalizerInfoDecoder : D.Decoder FinalizerInfo
finalizerInfoDecoder =
    D.succeed FinalizerInfo
        |> required "bakerId" D.int
        |> required "weight" D.int
        |> required "signed" D.bool


{-| The default Http.expectJson / Http.expectString don't allow you to see any body
returned in an error (i.e. 403) states. The following docs;
<https://package.elm-lang.org/packages/elm/http/latest/Http#expectStringResponse>
describe our sitution perfectly, so that's that code below, with a modified
Http.BadStatus\_ handler to map it to BadBody String instead of BadStatus Int
so we can actually see the error message if the JSON doesn't parse
-}
expectJson_ : (Result Http.Error a -> msg) -> D.Decoder a -> Expect msg
expectJson_ toMsg decoder =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (String.fromInt metadata.statusCode ++ ": " ++ body))

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (D.errorToString err))
