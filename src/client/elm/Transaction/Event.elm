module Transaction.Event exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Types as T



-- Based on https://gitlab.com/Concordium/consensus/globalstate-types/-/blob/master/src/Concordium/Types/Execution.hs#L353


type TransactionEvent
    = TransactionEventTransfer EventTransfer
    | TransactionEventAmountAddedByDecryption EventAmountAddedByDecryption
    | TransactionEventEncryptedSelfAmountAdded EventEncryptedSelfAmountAdded
      -- Enctrypted Transfers
    | TransactionEventNewEncryptedAmount EventNewEncryptedAmount
    | TransactionEventEncryptedAmountsRemoved EventEncryptedAmountsRemoved
      -- Accounts
    | TransactionEventAccountCreated EventAccountCreated
    | TransactionEventCredentialDeployed EventCredentialDeployed
    | TransactionEventAccountEncryptionKeyDeployed EventAccountEncryptionKeyDeployed
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
    | TransactionEventContractMessage EventContractMessage
      -- Core
    | TransactionEventElectionDifficultyUpdated EventElectionDifficultyUpdated
      -- Errors
    | TransactionEventRejected EventRejected


transactionEventsDecoder : D.Decoder TransactionEvent
transactionEventsDecoder =
    D.oneOf
        [ D.map TransactionEventTransfer eventTransferDecoder
        , D.map TransactionEventAmountAddedByDecryption eventAmountAddedByDecryptionDecoder
        , D.map TransactionEventEncryptedSelfAmountAdded eventEncryptedSelfAmountAddedDecoder

        -- Encrypted Transfers
        , D.map TransactionEventNewEncryptedAmount eventNewEncryptedAmountDecoder
        , D.map TransactionEventEncryptedAmountsRemoved eventEncryptedAmountsRemovedDecoder

        -- Accounts
        , D.map TransactionEventAccountCreated eventAccountCreatedDecoder
        , D.map TransactionEventCredentialDeployed eventCredentialDeployedDecoder
        , D.map TransactionEventAccountEncryptionKeyDeployed eventAccountEncryptionKeyDeployedDecoder

        -- Baking
        , D.map TransactionEventBakerAdded eventBakerAddedDecoder
        , D.map TransactionEventBakerRemoved eventBakerRemovedDecoder
        , D.map TransactionEventBakerStakeIncreased eventBakerStakeIncreasedDecoder
        , D.map TransactionEventBakerStakeDecreased eventBakerStakeDecreasedDecoder
        , D.map TransactionEventBakerSetRestakeEarnings eventBakerSetRestakeEarningsDecoder
        , D.map TransactionEventBakerKeysUpdated eventBakerKeysUpdatedDecoder

        -- Contracts
        , D.map TransactionEventModuleDeployed eventModuleDeployedDecoder
        , D.map TransactionEventContractInitialized eventContractInitializedDecoder
        , D.map TransactionEventContractMessage eventContractMessageDecoder

        -- Core
        , D.map TransactionEventElectionDifficultyUpdated eventElectionDifficultyUpdatedDecoder

        -- Error
        , D.map TransactionEventRejected eventRejectedDecoder
        ]



-- Transfers


type alias EventTransfer =
    { amount : T.Amount
    , tag : String
    , to : T.AccountInfo
    , from : T.AccountInfo
    }


eventTransferDecoder : D.Decoder EventTransfer
eventTransferDecoder =
    D.succeed EventTransfer
        |> required "amount" T.decodeAmount
        |> required "tag" (expectedTag "Transferred")
        |> required "to" T.accountInfoDecoder
        |> required "from" T.accountInfoDecoder


type alias EventEncryptedSelfAmountAdded =
    { account : String
    , amount : T.Amount
    , tag : String
    }


eventEncryptedSelfAmountAddedDecoder : D.Decoder EventEncryptedSelfAmountAdded
eventEncryptedSelfAmountAddedDecoder =
    D.succeed EventEncryptedSelfAmountAdded
        |> required "account" T.accountAddressDecoder
        |> required "amount" T.decodeAmount
        |> required "tag" (expectedTag "EncryptedSelfAmountAdded")


type alias EventAmountAddedByDecryption =
    { account : String
    , amount : T.Amount
    , tag : String
    }


eventAmountAddedByDecryptionDecoder : D.Decoder EventAmountAddedByDecryption
eventAmountAddedByDecryptionDecoder =
    D.succeed EventAmountAddedByDecryption
        |> required "account" T.accountAddressDecoder
        |> required "amount" T.decodeAmount
        |> required "tag" (expectedTag "AmountAddedByDecryption")



-- Encrypted Transfers


type alias EventNewEncryptedAmount =
    { account : String
    , tag : String
    }


eventNewEncryptedAmountDecoder : D.Decoder EventNewEncryptedAmount
eventNewEncryptedAmountDecoder =
    D.succeed EventNewEncryptedAmount
        |> required "account" T.accountAddressDecoder
        |> required "tag" (expectedTag "NewEncryptedAmount")


type alias EventEncryptedAmountsRemoved =
    { account : String
    , tag : String
    }


eventEncryptedAmountsRemovedDecoder : D.Decoder EventEncryptedAmountsRemoved
eventEncryptedAmountsRemovedDecoder =
    D.succeed EventEncryptedAmountsRemoved
        |> required "account" T.accountAddressDecoder
        |> required "tag" (expectedTag "EncryptedAmountsRemoved")



-- Accounts


type alias EventAccountCreated =
    { tag : String
    , account : String
    }


eventAccountCreatedDecoder : D.Decoder EventAccountCreated
eventAccountCreatedDecoder =
    D.succeed EventAccountCreated
        |> required "tag" (expectedTag "AccountCreated")
        |> required "contents" D.string


type alias EventCredentialDeployed =
    { tag : String
    , regid : String
    , account : String
    }


eventCredentialDeployedDecoder : D.Decoder EventCredentialDeployed
eventCredentialDeployedDecoder =
    D.succeed EventCredentialDeployed
        |> required "tag" (expectedTag "CredentialDeployed")
        |> required "regId" D.string
        |> required "account" T.accountAddressDecoder


type alias EventAccountEncryptionKeyDeployed =
    { key : String
    , account : String
    }


eventAccountEncryptionKeyDeployedDecoder : D.Decoder EventAccountEncryptionKeyDeployed
eventAccountEncryptionKeyDeployedDecoder =
    D.succeed EventAccountEncryptionKeyDeployed
        |> required "key" (expectedTag "AccountEncryptionKeyDeployed")
        |> required "account" T.accountAddressDecoder



-- Baking


type alias EventBakerAdded =
    { tag : String
    , bakerId : Int
    , signKey : String
    , electionKey : String
    , aggregationKey : String
    , stake : T.Amount
    , restakeEarnings : Bool
    }


eventBakerAddedDecoder : D.Decoder EventBakerAdded
eventBakerAddedDecoder =
    D.succeed EventBakerAdded
        |> required "tag" (expectedTag "BakerAdded")
        |> required "bakerId" D.int
        |> required "signKey" D.string
        |> required "electionKey" D.string
        |> required "aggregationKey" D.string
        |> required "stake" T.decodeAmount
        |> required "restakeEarnings" D.bool


type alias EventBakerRemoved =
    { tag : String
    , bakerId : Int
    , account : T.AccountAddress
    }


eventBakerRemovedDecoder : D.Decoder EventBakerRemoved
eventBakerRemovedDecoder =
    D.succeed EventBakerRemoved
        |> required "tag" (expectedTag "BakerRemoved")
        |> required "bakerId" D.int
        |> required "account" T.accountAddressDecoder


type alias EventBakerStakeIncreased =
    { tag : String
    , bakerId : Int
    , account : T.AccountAddress
    , newStake : T.Amount
    }


eventBakerStakeIncreasedDecoder : D.Decoder EventBakerStakeIncreased
eventBakerStakeIncreasedDecoder =
    D.succeed EventBakerStakeIncreased
        |> required "tag" (expectedTag "BakerStakeIncreased")
        |> required "bakerId" D.int
        |> required "account" T.accountAddressDecoder
        |> required "newStake" T.decodeAmount


type alias EventBakerStakeDecreased =
    { tag : String
    , bakerId : Int
    , account : T.AccountAddress
    , newStake : T.Amount
    }


eventBakerStakeDecreasedDecoder : D.Decoder EventBakerStakeDecreased
eventBakerStakeDecreasedDecoder =
    D.succeed EventBakerStakeDecreased
        |> required "tag" (expectedTag "BakerStakeDecreased")
        |> required "bakerId" D.int
        |> required "account" T.accountAddressDecoder
        |> required "newStake" T.decodeAmount


type alias EventBakerSetRestakeEarnings =
    { tag : String
    , bakerId : Int
    , account : T.AccountAddress
    , restakeEarnings : Bool
    }


eventBakerSetRestakeEarningsDecoder : D.Decoder EventBakerSetRestakeEarnings
eventBakerSetRestakeEarningsDecoder =
    D.succeed EventBakerSetRestakeEarnings
        |> required "tag" (expectedTag "BakerSetRestakeEarnings")
        |> required "bakerId" D.int
        |> required "account" T.accountAddressDecoder
        |> required "restakeEarnings" D.bool


type alias EventBakerKeysUpdated =
    { tag : String
    , bakerId : Int
    , account : T.AccountAddress
    , signKey : String
    , electionKey : String
    , aggregationKey : String
    }


eventBakerKeysUpdatedDecoder : D.Decoder EventBakerKeysUpdated
eventBakerKeysUpdatedDecoder =
    D.succeed EventBakerKeysUpdated
        |> required "tag" (expectedTag "BakerKeysUpdated")
        |> required "bakerId" D.int
        |> required "account" T.accountAddressDecoder
        |> required "signKey" D.string
        |> required "electionKey" D.string
        |> required "aggregationKey" D.string



-- Contracts


type alias EventModuleDeployed =
    { tag : String
    , contents : String
    }


eventModuleDeployedDecoder : D.Decoder EventModuleDeployed
eventModuleDeployedDecoder =
    D.succeed EventModuleDeployed
        |> required "tag" (expectedTag "ModuleDeployed")
        |> required "contents" D.string


type alias EventContractInitialized =
    { tag : String
    , amount : T.Amount
    , address : T.ContractAddress
    , name : Int
    , ref : String
    }


eventContractInitializedDecoder : D.Decoder EventContractInitialized
eventContractInitializedDecoder =
    D.succeed EventContractInitialized
        |> required "tag" (expectedTag "ContractInitialized")
        |> required "amount" T.decodeAmount
        |> required "address" T.contractAddressDecoder
        |> required "name" D.int
        |> required "ref" D.string


type alias EventContractMessage =
    { tag : String
    , amount : T.Amount
    , address : T.ContractAddress
    , message : String
    }


eventContractMessageDecoder : D.Decoder EventContractMessage
eventContractMessageDecoder =
    D.succeed EventContractMessage
        |> required "tag" (expectedTag "Updated")
        |> required "amount" T.decodeAmount
        |> required "address" T.contractAddressDecoder
        |> required "message" D.string



-- Core


type alias EventElectionDifficultyUpdated =
    { tag : String
    , difficulty : Int
    }


eventElectionDifficultyUpdatedDecoder : D.Decoder EventElectionDifficultyUpdated
eventElectionDifficultyUpdatedDecoder =
    D.succeed EventElectionDifficultyUpdated
        |> required "tag" (expectedTag "ElectionDifficultyUpdated")
        |> required "difficulty" D.int



-- Errors


type alias EventRejected =
    -- @TODO swap to camel case
    { transactionType : String
    , reason : String
    , hash : String
    }


eventRejectedDecoder : D.Decoder EventRejected
eventRejectedDecoder =
    D.succeed EventRejected
        |> required "transactionType" D.string
        |> required "reason" D.string
        |> required "hash" D.string



-- Helpers


expectedTag : String -> D.Decoder String
expectedTag expected =
    D.string
        |> D.andThen
            (\s ->
                if s == expected then
                    D.succeed s

                else
                    D.fail <| "does not match expected tag " ++ s
            )
