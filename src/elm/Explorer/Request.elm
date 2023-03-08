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
    , pendingUpdates : List EventUpdateEnqueued
    , chainParameters : ChainParameters
    }


blockSummaryDecoder : D.Decoder BlockSummary
blockSummaryDecoder =
    D.succeed BlockSummary
        |> required "specialEvents" (D.list specialEventDecoder)
        |> required "transactionSummaries" (D.list transactionSummaryDecoder)
        |> optional "finalizationData" (D.nullable finalizationDataDecoder) Nothing
        |> required "pendingUpdates" (D.list pendingUpdateDecoder)
        |> required "chainParameters" chainParametersDecoder



-- Updates


type alias ChainParameters =
    { rewardParameters : RewardParameters
    }


type alias Updates =
    { chainParameters : ChainParameters
    , updateQueues : UpdateQueues
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


chainParametersDecoder : D.Decoder ChainParameters
chainParametersDecoder =
    D.succeed ChainParameters
        |> required "rewardParameters" rewardParametersDecoder


rewardParametersDecoder : D.Decoder RewardParameters
rewardParametersDecoder =
    D.succeed RewardParameters
        |> required "mintDistribution"
            (D.oneOf
                [ mintDistributionV1Decoder
                , mintDistributionV0Decoder
                ]
            )
        |> required "transactionFeeDistribution" transactionFeeDistributionDecoder
        |> required "gasRewards" gasRewardsDecoder



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
    , passiveReward : T.Amount
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
                        |> required "passiveReward" T.decodeAmount
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
