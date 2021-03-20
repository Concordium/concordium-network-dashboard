module Explorer.Request exposing (..)

import Api
import Http exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Task
import Time exposing (Posix)
import Transaction.Event exposing (..)
import Transaction.Summary exposing (..)
import Types as T


type alias Config =
    { middlewareUrl : String
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
        , expect = Api.expectJson_ msg blockSummaryDecoder
        }


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
    , authorizations : Authorizations
    , updateQueues : UpdateQueues
    }


type alias ChainParameters =
    { rewardParameters : RewardParameters
    , microGTUPerEuro : Relation
    , foundationAccountIndex : Int
    , accountCreationLimit : Int
    , bakerCooldownEpochs : Int
    , electionDifficulty : Float
    , euroPerEnergy : Relation
    , minimumThresholdForBaking : T.Amount
    }


type alias RewardParameters =
    { mintDistribution : MintDistribution
    , transactionFeeDistribution : TransactionFeeDistribution
    , gasRewards : GasRewards
    }


type alias UpdateQueues =
    { mintDistribution : UpdateQueue MintDistribution
    , transactionFeeDistribution : UpdateQueue TransactionFeeDistribution
    , authorization : UpdateQueue Authorizations
    , microGTUPerEuro : UpdateQueue Relation
    , protocol : UpdateQueue ProtocolUpdate
    , gasRewards : UpdateQueue GasRewards
    , foundationAccount : UpdateQueue T.AccountAddress
    , electionDifficulty : UpdateQueue Float
    , euroPerEnergy : UpdateQueue Relation
    , bakerStakeThreshold : UpdateQueue T.Amount
    }


type alias UpdateQueue a =
    { nextSequenceNumber : Int
    , queue : List (UpdateQueueItem a)
    }


type alias UpdateQueueItem a =
    { effectiveTime : Posix
    , update : a
    }


updatesDecoder : D.Decoder Updates
updatesDecoder =
    D.succeed Updates
        |> required "chainParameters" chainParametersDecoder
        |> required "authorizations" authorizationsDecoder
        |> required "updateQueues" updateQueuesDecoder


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
        |> required "minimumThresholdForBaking" T.decodeAmount


rewardParametersDecoder : D.Decoder RewardParameters
rewardParametersDecoder =
    D.succeed RewardParameters
        |> required "mintDistribution" mintDistributionDecoder
        |> required "transactionFeeDistribution" transactionFeeDistributionDecoder
        |> required "gASRewards" gasRewardsDecoder


updateQueuesDecoder : D.Decoder UpdateQueues
updateQueuesDecoder =
    D.succeed UpdateQueues
        |> required "mintDistribution" (updateQueueDecoder mintDistributionDecoder)
        |> required "transactionFeeDistribution" (updateQueueDecoder transactionFeeDistributionDecoder)
        |> required "authorization" (updateQueueDecoder authorizationsDecoder)
        |> required "microGTUPerEuro" (updateQueueDecoder relationDecoder)
        |> required "protocol" (updateQueueDecoder protocolUpdateDecoder)
        |> required "gasRewards" (updateQueueDecoder gasRewardsDecoder)
        |> required "foundationAccount" (updateQueueDecoder T.accountAddressDecoder)
        |> required "electionDifficulty" (updateQueueDecoder D.float)
        |> required "euroPerEnergy" (updateQueueDecoder relationDecoder)
        |> required "bakerStakeThreshold" (updateQueueDecoder T.decodeAmount)


updateQueueDecoder : D.Decoder a -> D.Decoder (UpdateQueue a)
updateQueueDecoder decoder =
    D.succeed UpdateQueue
        |> required "nextSequenceNumber" D.int
        |> required "queue" (D.list (updateQueueItemDecoder decoder))


updateQueueItemDecoder : D.Decoder a -> D.Decoder (UpdateQueueItem a)
updateQueueItemDecoder decoder =
    D.succeed UpdateQueueItem
        |> required "effectiveTime" (D.map (\seconds -> Time.millisToPosix (seconds * 1000)) D.int)
        |> required "update" decoder



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
