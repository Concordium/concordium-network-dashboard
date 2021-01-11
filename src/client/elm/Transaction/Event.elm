module Transaction.Event exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Time exposing (Posix)
import Types as T



-- Based on https://gitlab.com/Concordium/consensus/globalstate-types/-/blob/master/src/Concordium/Types/Execution.hs#L353


type TransactionEvent
    = TransactionEventTransferred EventTransferred
    | TransactionEventTransferredWithSchedule EventTransferredWithSchedule
    | TransactionEventAmountAddedByDecryption EventAmountAddedByDecryption
    | TransactionEventEncryptedSelfAmountAdded EventEncryptedSelfAmountAdded
      -- Encrypted Transfers
    | TransactionEventNewEncryptedAmount EventNewEncryptedAmount
    | TransactionEventEncryptedAmountsRemoved EventEncryptedAmountsRemoved
      -- Accounts
    | TransactionEventAccountCreated EventAccountCreated
    | TransactionEventCredentialDeployed EventCredentialDeployed
      -- Account Keys
    | TransactionEventAccountKeysUpdated
    | TransactionEventAccountKeysAdded
    | TransactionEventAccountKeysRemoved
    | TransactionEventAccountKeysSignThresholdUpdated
      -- Baking
    | TransactionEventBakerAdded EventBakerAdded
    | TransactionEventBakerRemoved EventBakerRemoved
    | TransactionEventBakerStakeIncreased EventBakerStakeIncreased
    | TransactionEventBakerStakeDecreased EventBakerStakeDecreased
    | TransactionEventBakerSetRestakeEarnings EventBakerSetRestakeEarnings
    | TransactionEventBakerKeysUpdated EventBakerKeysUpdated
      -- Contracts
    | TransactionEventModuleDeployed EventModuleDeployed
    | TransactionEventContractInitialized EventContractInitialized
    | TransactionEventContractUpdated EventContractUpdated
      -- Core
    | TransactionEventUpdateEnqueued EventUpdateEnqueued
      -- Errors
    | TransactionEventUnknownTag String



-- Transfers


type alias EventTransferred =
    { amount : T.Amount
    , to : T.Address
    , from : T.Address
    }


type alias EventTransferredWithSchedule =
    { releaseSchedule : T.ReleaseSchedule
    , from : T.AccountAddress
    , to : T.AccountAddress
    }


type alias EventAmountAddedByDecryption =
    { account : T.AccountAddress
    , amount : T.Amount
    }


{-| Haskell version also has field "newAmount".
-}
type alias EventEncryptedSelfAmountAdded =
    { account : T.AccountAddress
    , amount : T.Amount
    }



-- Encrypted Transfers


{-| Haskell version also has fields "newIndex" and "encryptedAmount".
-}
type alias EventNewEncryptedAmount =
    { account : String }


{-| Haskell version also has fields "newAmount", "inputAmount", and "upToIndex".
-}
type alias EventEncryptedAmountsRemoved =
    { account : String }



-- Accounts


type alias EventAccountCreated =
    { account : String }


type alias EventCredentialDeployed =
    { regid : String
    , account : String
    }



-- Baking


type alias EventBakerAdded =
    { bakerId : Int
    , account : T.AccountAddress
    , signKey : String
    , electionKey : String
    , aggregationKey : String
    , stake : T.Amount
    , restakeEarnings : Bool
    }


type alias EventBakerRemoved =
    { bakerId : Int
    , account : T.AccountAddress
    }


type alias EventBakerStakeIncreased =
    { bakerId : Int
    , account : T.AccountAddress
    , newStake : T.Amount
    }


type alias EventBakerStakeDecreased =
    { bakerId : Int
    , account : T.AccountAddress
    , newStake : T.Amount
    }


type alias EventBakerSetRestakeEarnings =
    { bakerId : Int
    , account : T.AccountAddress
    , restakeEarnings : Bool
    }


type alias EventBakerKeysUpdated =
    { bakerId : Int
    , account : T.AccountAddress
    , signKey : String
    , electionKey : String
    , aggregationKey : String
    }



-- Contracts


type alias EventModuleDeployed =
    { contents : String }


type alias EventContractInitialized =
    { ref : String
    , address : T.ContractAddress
    , amount : T.Amount
    , contractName : String
    , events : List T.ContractEvent
    }


type alias EventContractUpdated =
    { address : T.ContractAddress
    , instigator : T.Address
    , amount : T.Amount
    , message : String
    , contractName : String
    , functionName : String
    , events : List T.ContractEvent
    }



-- Core


type alias EventUpdateEnqueued =
    { effectiveTime : Posix
    , payload : UpdatePayload
    }


type UpdatePayload
    = MintDistributionPayload MintDistribution
    | TransactionFeeDistributionPayload TransactionFeeDistribution
    | GasRewardsPayload GasRewards
    | ElectionDifficultyPayload Float
    | EuroPerEnergyPayload Float
    | MicroGtuPerEnergyPayload Int
    | FoundationAccountPayload T.AccountAddress
    | AuthorizationPayload Authorization


type alias MintDistribution =
    { mintPerSlot : Float
    , bakingReward : Float
    , finalizationReward : Float
    }


type alias TransactionFeeDistribution =
    { gasAccount : Float
    , baker : Float
    }


type alias GasRewards =
    { chainUpdate : Float
    , accountCreation : Float
    , baker : Float
    , finalizationProof : Float
    }


type alias Authorization =
    {}



-- type alias Authorization =
--     { keys : List AuthorizationKey
--     , emergency : AuthorizationAccess
--     , authorization : AuthorizationAccess
--     , protocol : AuthorizationAccess
--     , electionDifficulty : AuthorizationAccess
--     , euroPerEnergy : AuthorizationAccess
--     , microGTUPerEuro : AuthorizationAccess
--     , foundationAccount : AuthorizationAccess
--     , mintDistribution : AuthorizationAccess
--     , transactionFeeDistribution : AuthorizationAccess
--     , paramGASRewards : AuthorizationAccess
--     }
-- type alias KeyIndex =
--     Int
-- type alias AuthorizationKey =
--     { schemeId : String
--     , verifyKey : String
--     }
-- type alias AuthorizationAccess =
--     { authorizedKeys : List KeyIndex
--     , threshold : Int
--     }
-- Errors


type alias EventRejected =
    { transactionType : String
    , reason : String
    , hash : String
    }


updatePayloadDecoder : D.Decoder UpdatePayload
updatePayloadDecoder =
    let
        decode updateType =
            D.field "update" <|
                case updateType of
                    "mintDistribution" ->
                        mintDistributionDecoder |> D.map MintDistributionPayload

                    "transactionFeeDistribution" ->
                        transactionFeeDistributionDecoder |> D.map TransactionFeeDistributionPayload

                    "gASRewards" ->
                        gasRewardsDecoder |> D.map GasRewardsPayload

                    "electionDifficulty" ->
                        D.float |> D.map ElectionDifficultyPayload

                    "euroPerEnergy" ->
                        D.float |> D.map EuroPerEnergyPayload

                    "microGTUPerEuro" ->
                        D.int |> D.map MicroGtuPerEnergyPayload

                    "foundationAccount" ->
                        T.accountAddressDecoder |> D.map FoundationAccountPayload

                    "authorization" ->
                        authorizationDecoder |> D.map AuthorizationPayload

                    _ ->
                        D.fail "Unknown update type"
    in
    D.field "updateType" D.string |> D.andThen decode


mintDistributionDecoder : D.Decoder MintDistribution
mintDistributionDecoder =
    D.succeed MintDistribution
        |> required "mintPerSlot" D.float
        |> required "bakingReward" D.float
        |> required "finalizationReward" D.float


transactionFeeDistributionDecoder : D.Decoder TransactionFeeDistribution
transactionFeeDistributionDecoder =
    D.succeed TransactionFeeDistribution
        |> required "gasAccount" D.float
        |> required "baker" D.float


gasRewardsDecoder : D.Decoder GasRewards
gasRewardsDecoder =
    D.succeed GasRewards
        |> required "chainUpdate" D.float
        |> required "accountCreation" D.float
        |> required "baker" D.float
        |> required "finalizationProof" D.float


authorizationDecoder : D.Decoder Authorization
authorizationDecoder =
    D.succeed Authorization


contractInitNameDecoder : D.Decoder String
contractInitNameDecoder =
    D.string
        |> D.andThen
            (\str ->
                if String.startsWith "init_" str then
                    D.succeed <| String.dropLeft 5 str

                else
                    D.fail "Invalid init function name"
            )


contractReceiveNameDecoder : D.Decoder { contractName : String, functionName : String }
contractReceiveNameDecoder =
    D.string
        |> D.andThen
            (\str ->
                let
                    parts =
                        String.split "." str
                in
                case ( List.head parts, List.tail parts ) of
                    ( Just contractName, Just functionNameParts ) ->
                        D.succeed <| { contractName = contractName, functionName = String.join "." functionNameParts }

                    _ ->
                        D.fail "Invalid receive function name"
            )


transactionEventsDecoder : D.Decoder TransactionEvent
transactionEventsDecoder =
    let
        decode tag =
            case tag of
                "Transferred" ->
                    D.succeed EventTransferred
                        |> required "amount" T.decodeAmount
                        |> required "to" T.addressDecoder
                        |> required "from" T.addressDecoder
                        |> D.map TransactionEventTransferred

                "TransferredWithSchedule" ->
                    D.succeed EventTransferredWithSchedule
                        |> required "amount" T.releaseScheduleDecoder
                        |> required "from" T.accountAddressDecoder
                        |> required "to" T.accountAddressDecoder
                        |> D.map TransactionEventTransferredWithSchedule

                "AmountAddedByDecryption" ->
                    D.succeed EventAmountAddedByDecryption
                        |> required "account" T.accountAddressDecoder
                        |> required "amount" T.decodeAmount
                        |> D.map TransactionEventAmountAddedByDecryption

                "EncryptedSelfAmountAdded" ->
                    D.succeed EventEncryptedSelfAmountAdded
                        |> required "account" T.accountAddressDecoder
                        |> required "amount" T.decodeAmount
                        |> D.map TransactionEventEncryptedSelfAmountAdded

                -- Encrypted Transfers
                "NewEncryptedAmount" ->
                    D.succeed EventNewEncryptedAmount
                        |> required "account" T.accountAddressDecoder
                        |> D.map TransactionEventNewEncryptedAmount

                "EncryptedAmountsRemoved" ->
                    D.succeed EventEncryptedAmountsRemoved
                        |> required "account" T.accountAddressDecoder
                        |> D.map TransactionEventEncryptedAmountsRemoved

                -- Accounts
                "AccountCreated" ->
                    D.succeed EventAccountCreated
                        |> required "contents" D.string
                        |> D.map TransactionEventAccountCreated

                "CredentialDeployed" ->
                    D.succeed EventCredentialDeployed
                        |> required "regId" D.string
                        |> required "account" T.accountAddressDecoder
                        |> D.map TransactionEventCredentialDeployed

                -- Account Keys
                "AccountKeysUpdated" ->
                    D.succeed TransactionEventAccountKeysUpdated

                "AccountKeysAdded" ->
                    D.succeed TransactionEventAccountKeysAdded

                "AccountKeysRemoved" ->
                    D.succeed TransactionEventAccountKeysRemoved

                "AccountKeysSignThresholdUpdated" ->
                    D.succeed TransactionEventAccountKeysSignThresholdUpdated

                -- Baking
                "BakerAdded" ->
                    D.succeed EventBakerAdded
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "signKey" D.string
                        |> required "electionKey" D.string
                        |> required "aggregationKey" D.string
                        |> required "stake" T.decodeAmount
                        |> required "restakeEarnings" D.bool
                        |> D.map TransactionEventBakerAdded

                "BakerRemoved" ->
                    D.succeed EventBakerRemoved
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> D.map TransactionEventBakerRemoved

                "BakerStakeIncreased" ->
                    D.succeed EventBakerStakeIncreased
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "newStake" T.decodeAmount
                        |> D.map TransactionEventBakerStakeIncreased

                "BakerStakeDecreased" ->
                    D.succeed EventBakerStakeDecreased
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "newStake" T.decodeAmount
                        |> D.map TransactionEventBakerStakeDecreased

                "BakerSetRestakeEarnings" ->
                    D.succeed EventBakerSetRestakeEarnings
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "restakeEarnings" D.bool
                        |> D.map TransactionEventBakerSetRestakeEarnings

                "BakerKeysUpdated" ->
                    D.succeed EventBakerKeysUpdated
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "signKey" D.string
                        |> required "electionKey" D.string
                        |> required "aggregationKey" D.string
                        |> D.map TransactionEventBakerKeysUpdated

                -- Contracts
                "ModuleDeployed" ->
                    D.succeed EventModuleDeployed
                        |> required "contents" D.string
                        |> D.map TransactionEventModuleDeployed

                "ContractInitialized" ->
                    D.succeed EventContractInitialized
                        |> required "ref" D.string
                        |> required "address" T.contractAddressDecoder
                        |> required "amount" T.decodeAmount
                        |> required "initName" contractInitNameDecoder
                        |> required "events" (D.list T.contractEventDecoder)
                        |> D.map TransactionEventContractInitialized

                "Updated" ->
                    D.succeed EventContractUpdated
                        |> required "address" T.contractAddressDecoder
                        |> required "instigator" T.addressDecoder
                        |> required "amount" T.decodeAmount
                        |> required "message" D.string
                        |> required "receiveName" (D.map .contractName contractReceiveNameDecoder)
                        |> required "receiveName" (D.map .functionName contractReceiveNameDecoder)
                        |> required "events" (D.list T.contractEventDecoder)
                        |> D.map TransactionEventContractUpdated

                -- Core
                "UpdateEnqueued" ->
                    D.succeed EventUpdateEnqueued
                        |> required "effectiveTime" (D.map (\seconds -> Time.millisToPosix (seconds * 1000)) D.int)
                        |> required "payload" updatePayloadDecoder
                        |> D.map TransactionEventUpdateEnqueued

                -- Errors
                unknownTag ->
                    D.succeed <| TransactionEventUnknownTag unknownTag
    in
    D.field "tag" D.string |> D.andThen decode
