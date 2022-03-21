module Explorer.Request exposing (..)

import Api
import Dict
import Explorer.Stubs as Stubs
import Http exposing (..)
import Http.Mock as Mock
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Task
import Time exposing (Posix)
import Transaction.Event exposing (..)
import Transaction.Summary exposing (..)
import Types as T


-- BlockSummary


getBlockSummary : String -> (Result Error BlockSummary -> msg) -> Cmd msg
getBlockSummary blockhash msg =
    Http.get
        { url = "/v1/blockSummary/" ++ blockhash
        , expect = Api.expectJson_ msg blockSummaryDecoder

        -- for testing, uncomment to use a stub instead of the real response
        --, expect = Mock.expectJson mockBlockSummaryResponse msg blockSummaryDecoder
        }



{--| for testing, uncomment to use a stub instead of the real response
mockBlockSummaryResponse =
    let
        metadata =
            { url = "fakeurl.com"
            , statusCode = 200
            , statusText = ""
            , headers = Dict.empty
            }
    in
    Http.GoodStatus_ metadata Stubs.getBlockSummaryLongMemoStub
--}


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
        |> required "updates" (D.oneOf [updatesDecoderV1, updatesDecoderV0])

-- Updates

type ChainParameters = CPV0 ChainParametersV0 | CPV1 ChainParametersV1

type alias Updates =
    { chainParameters : ChainParameters
    , keyCollection : UpdateKeysCollection
    , updateQueues : UpdateQueues
    }

type alias ChainParametersV0 =
    { rewardParameters : RewardParameters
    , microCCDPerEuro : Relation
    , foundationAccountIndex : Int
    , accountCreationLimit : Int
    , bakerCooldownEpochs : Int
    , electionDifficulty : Float
    , euroPerEnergy : Relation
    , minimumThresholdForBaking : T.Amount
    }
    
type alias ChainParametersV1 =
    { rewardParameters : RewardParameters
    , microCCDPerEuro : Relation
    , foundationAccountIndex : Int
    , accountCreationLimit : Int
    , electionDifficulty : Float
    , euroPerEnergy : Relation
    , poolOwnerCooldown : Int
    , delegatorCooldown : Int
    , finalizationCommissionLPool : Float
    , bakingCommissionLPool : Float
    , transactionCommissiionLPool : Float
    , finalizationCommissionRange : Range Float
    , bakingCommissionRange : Range Float
    , transactionCommissionRange : Range Float
    , minimumEquityCapital : T.Amount
    , capitalBound : Float
    , leverageBound : Relation
    , rewardPeriodLength : Int
    , mintPerPayday : Float
    }


type alias RewardParameters =
    { mintDistribution : MintDistribution
    , transactionFeeDistribution : TransactionFeeDistribution
    , gasRewards : GasRewards
    }


type alias UpdateQueues =
    { mintDistribution : UpdateQueue MintDistribution
    , transactionFeeDistribution : UpdateQueue TransactionFeeDistribution
    , rootKeys : UpdateQueue HigherLevelKeys
    , level1Keys : UpdateQueue HigherLevelKeys
    , level2Keys : UpdateQueue Authorizations
    , microCCDPerEuro : UpdateQueue Relation
    , protocol : UpdateQueue ProtocolUpdate
    , gasRewards : UpdateQueue GasRewards
    , foundationAccount : UpdateQueue Int
    , electionDifficulty : UpdateQueue Float
    , euroPerEnergy : UpdateQueue Relation
    , anonymityRevoker : UpdateQueue AnonymityRevokerInfo
    , identityProvider : UpdateQueue IdentityProviderInfo
    , poolParameters : UpdateQueue PoolParameters
    , cooldownParameters : UpdateQueue CooldownParameters
    , timeParameters : UpdateQueue TimeParameters    
    }


type alias UpdateQueue a =
    { nextSequenceNumber : Int
    , queue : List (UpdateQueueItem a)
    }


type alias UpdateQueueItem a =
    { effectiveTime : Posix
    , update : a
    }

updatesDecoderV0 : D.Decoder Updates
updatesDecoderV0 =
    D.succeed Updates
        |> required "chainParameters" chainParametersDecoderV0
        |> required "keys" updateKeysCollectionDecoder
        |> required "updateQueues" updateQueuesDecoder
    
updatesDecoderV1 : D.Decoder Updates
updatesDecoderV1 =
    D.succeed Updates
        |> required "chainParameters" chainParametersDecoderV1
        |> required "keys" updateKeysCollectionDecoder
        |> required "updateQueues" updateQueuesDecoder

chainParametersDecoderV0 : D.Decoder ChainParameters
chainParametersDecoderV0 =
    D.succeed ChainParametersV0
        |> required "rewardParameters" rewardParametersDecoder
        |> required "microGTUPerEuro" relationDecoder
        |> required "foundationAccountIndex" D.int
        |> required "accountCreationLimit" D.int
        |> required "bakerCooldownEpochs" D.int   
        |> required "electionDifficulty" D.float
        |> required "euroPerEnergy" relationDecoder
        |> required "minimumThresholdForBaking" T.decodeAmount
        |> D.map CPV0
           
chainParametersDecoderV1 : D.Decoder ChainParameters
chainParametersDecoderV1 =
    D.succeed ChainParametersV1
        |> required "rewardParameters" rewardParametersDecoder
        |> required "microGTUPerEuro" relationDecoder
        |> required "foundationAccountIndex" D.int
        |> required "accountCreationLimit" D.int
        |> required "electionDifficulty" D.float
        |> required "euroPerEnergy" relationDecoder
        |> required "poolOwnerCooldown" D.int
        |> required "delegatorCooldown" D.int
        |> required "finalizationCommissionLPool" D.float
        |> required "bakingCommissionLPool" D.float
        |> required "transactionCommissionLPool" D.float
        |> required "finalizationCommissionRange" (rangeDecoder D.float)
        |> required "bakingCommissionRange" (rangeDecoder D.float)
        |> required "transactionCommissionRange" (rangeDecoder D.float)
        |> required "minimumEquityCapital" T.decodeAmount
        |> required "capitalBound" D.float
        |> required "leverageBound" relationDecoder
        |> required "rewardPeriodLength" D.int
        |> required "mintPerPayday" D.float
        |> D.map CPV1

rewardParametersDecoder : D.Decoder RewardParameters
rewardParametersDecoder =
    D.succeed RewardParameters
        |> required "mintDistribution" (D.oneOf [ mintDistributionV1Decoder
                                                , mintDistributionV0Decoder] )
        |> required "transactionFeeDistribution" transactionFeeDistributionDecoder
        |> required "gASRewards" gasRewardsDecoder

updateQueuesDecoder : D.Decoder UpdateQueues
updateQueuesDecoder =
    D.succeed UpdateQueues
        |> required "mintDistribution" (updateQueueDecoder (D.oneOf [ mintDistributionV1Decoder
                                                                    , mintDistributionV0Decoder ]))
        |> required "transactionFeeDistribution" (updateQueueDecoder transactionFeeDistributionDecoder)
        |> required "rootKeys" (updateQueueDecoder higherLevelKeysDecoder)
        |> required "level1Keys" (updateQueueDecoder higherLevelKeysDecoder)
        |> required "level2Keys" (updateQueueDecoder authorizationsDecoder)
        |> required "microGTUPerEuro" (updateQueueDecoder relationDecoder)
        |> required "protocol" (updateQueueDecoder protocolUpdateDecoder)
        |> required "gasRewards" (updateQueueDecoder gasRewardsDecoder)
        |> required "foundationAccount" (updateQueueDecoder D.int)
        |> required "electionDifficulty" (updateQueueDecoder D.float)
        |> required "euroPerEnergy" (updateQueueDecoder relationDecoder)
        |> required "addAnonymityRevoker" (updateQueueDecoder arDecoder)
        |> required "addIdentityProvider" (updateQueueDecoder ipDecoder)
        |> optional "poolParameters" (updateQueueDecoder (D.oneOf [ poolParametersV1Decoder
                                                                 , poolParametersV0Decoder ]))
           { nextSequenceNumber = 0, queue = [] }
        |> optional "cooldownParameters" (updateQueueDecoder (D.oneOf [ cooldownParametersV1Decoder
                                                                      , cooldownParametersV0Decoder]))
           { nextSequenceNumber = 0, queue = [] }
        |> optional "timeParameters" (updateQueueDecoder timeParametersDecoder) { nextSequenceNumber = 0, queue = [] }


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
    | SpecialEventPaydayFoundationReward PaydayFoundationReward
    | SpecialEventPaydayAccountReward PaydayAccountReward
    | SpecialEventBlockAccrueReward BlockAccrueReward
    | SpecialEventPaydayPoolReward PaydayPoolReward


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

type alias PaydayFoundationReward =
    { foundationAccount : T.AccountAddress
    , developmentCharge : T.Amount
    }

type alias PaydayAccountReward =
    { account : T.AccountAddress
    , transactionFees : T.Amount
    , bakerReward : T.Amount
    , finalizationReward : T.Amount
    }

type alias BlockAccrueReward =
    { transactionFees : T.Amount
    , oldGASAccount : T.Amount
    , newGASAccount : T.Amount
    , bakerReward : T.Amount
    , lPoolReward : T.Amount
    , foundationCharge : T.Amount
    , bakerId : T.BakerId
    }

type alias PaydayPoolReward =
    { poolOwner : Maybe T.BakerId
    , transactionFees : T.Amount
    , bakerReward : T.Amount
    , finalizationReward : T.Amount
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

                "PaydayFoundationReward" ->
                    D.succeed PaydayFoundationReward
                        |> required "foundationAccount" T.accountAddressDecoder
                        |> required "developmentCharge" T.decodeAmount
                        |> D.map SpecialEventPaydayFoundationReward

                "PaydayAccountReward" ->
                    D.succeed PaydayAccountReward
                        |> required "account" T.accountAddressDecoder
                        |> required "transactionFees" T.decodeAmount
                        |> required "bakerReward" T.decodeAmount
                        |> required "finalizationReward" T.decodeAmount
                        |> D.map SpecialEventPaydayAccountReward

                "BlockAccrueReward" ->
                    D.succeed BlockAccrueReward
                      |> required "transactionFees" T.decodeAmount
                      |> required "oldGASAccount" T.decodeAmount
                      |> required "newGASAccount" T.decodeAmount
                      |> required "bakerReward" T.decodeAmount
                      |> required "lPoolReward" T.decodeAmount
                      |> required "foundationCharge" T.decodeAmount
                      |> required "bakerId" D.int
                      |> D.map SpecialEventBlockAccrueReward

                "PaydayPoolReward" ->
                    D.succeed PaydayPoolReward
                      |> required "poolOwner" (D.nullable D.int)
                      |> required "transactionFees" T.decodeAmount
                      |> required "bakerReward" T.decodeAmount
                      |> required "finalizationReward" T.decodeAmount
                      |> D.map SpecialEventPaydayPoolReward

                _ ->
                    D.fail """Invalid SpecialEvent tag.
                              Expected one of the following: BakingRewards, Mint,
                              FinalizationRewards, BlockReward,
                              PaydayFoundationReward, PaydayAccountReward,
                              BlockAccrueReward, PaydayPoolReward."""
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
