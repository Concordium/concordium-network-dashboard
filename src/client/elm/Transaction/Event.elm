module Transaction.Event exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Transaction.Amount exposing (Amount, decodeAmount)



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
    | TransactionEventBakerAccountUpdated EventBakerAccountUpdated
    | TransactionEventBakerKeyUpdated EventBakerKeyUpdated
    | TransactionEventBakerElectionKeyUpdated EventBakerElectionKeyUpdated
      -- Contracts
    | TransactionEventModuleDeployed EventModuleDeployed
    | TransactionEventContractInitialized EventContractInitialized
    | TransactionEventContractMessage EventContractMessage
      -- Delegation
    | TransactionEventStakeDelegated EventStakeDelegated
    | TransactionEventStakeUndelegated EventStakeUndelegated
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
        , D.map TransactionEventBakerAccountUpdated eventBakerAccountUpdatedDecoder
        , D.map TransactionEventBakerKeyUpdated eventBakerKeyUpdatedDecoder
        , D.map TransactionEventBakerElectionKeyUpdated eventBakerElectionKeyUpdatedDecoder

        -- Contracts
        , D.map TransactionEventModuleDeployed eventModuleDeployedDecoder
        , D.map TransactionEventContractInitialized eventContractInitializedDecoder
        , D.map TransactionEventContractMessage eventContractMessageDecoder

        -- Delegation
        , D.map TransactionEventStakeDelegated eventStakeDelegatedDecoder
        , D.map TransactionEventStakeUndelegated eventStakeUndelegatedDecoder

        -- Core
        , D.map TransactionEventElectionDifficultyUpdated eventElectionDifficultyUpdatedDecoder

        -- Error
        , D.map TransactionEventRejected eventRejectedDecoder
        ]



-- Transfers


type alias EventTransfer =
    { amount : Amount
    , tag : String
    , to : AccountInfo
    , from : AccountInfo
    }


eventTransferDecoder : D.Decoder EventTransfer
eventTransferDecoder =
    D.succeed EventTransfer
        |> required "amount" decodeAmount
        |> required "tag" (expectedTag "Transferred")
        |> required "to" accountInfoDecoder
        |> required "from" accountInfoDecoder


type alias EventEncryptedSelfAmountAdded =
    { account : String
    , amount : Amount
    , tag : String
    }


eventEncryptedSelfAmountAddedDecoder : D.Decoder EventEncryptedSelfAmountAdded
eventEncryptedSelfAmountAddedDecoder =
    D.succeed EventEncryptedSelfAmountAdded
        |> required "account" D.string
        |> required "amount" decodeAmount
        |> required "tag" (expectedTag "EncryptedSelfAmountAdded")


type alias EventAmountAddedByDecryption =
    { account : String
    , amount : Amount
    , tag : String
    }


eventAmountAddedByDecryptionDecoder : D.Decoder EventAmountAddedByDecryption
eventAmountAddedByDecryptionDecoder =
    D.succeed EventAmountAddedByDecryption
        |> required "account" D.string
        |> required "amount" decodeAmount
        |> required "tag" (expectedTag "AmountAddedByDecryption")



-- Encrypted Transfers


type alias EventNewEncryptedAmount =
    { account : String
    , tag : String
    }


eventNewEncryptedAmountDecoder : D.Decoder EventNewEncryptedAmount
eventNewEncryptedAmountDecoder =
    D.succeed EventNewEncryptedAmount
        |> required "account" D.string
        |> required "tag" (expectedTag "NewEncryptedAmount")


type alias EventEncryptedAmountsRemoved =
    { account : String
    , tag : String
    }


eventEncryptedAmountsRemovedDecoder : D.Decoder EventEncryptedAmountsRemoved
eventEncryptedAmountsRemovedDecoder =
    D.succeed EventEncryptedAmountsRemoved
        |> required "account" D.string
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
        |> required "account" D.string


type alias EventAccountEncryptionKeyDeployed =
    { key : String
    , account : String
    }


eventAccountEncryptionKeyDeployedDecoder : D.Decoder EventAccountEncryptionKeyDeployed
eventAccountEncryptionKeyDeployedDecoder =
    D.succeed EventAccountEncryptionKeyDeployed
        |> required "key" (expectedTag "AccountEncryptionKeyDeployed")
        |> required "account" D.string



-- Baking


type alias EventBakerAdded =
    { tag : String
    , bakerId : Int
    }


eventBakerAddedDecoder : D.Decoder EventBakerAdded
eventBakerAddedDecoder =
    D.succeed EventBakerAdded
        |> required "tag" (expectedTag "BakerAdded")
        |> required "contents" D.int


type AccountInfo
    = AddressAccount String
    | AddressContract String
    | AddressUnknown


accountInfoAddress : AccountInfo -> String
accountInfoAddress accountInfo =
    case accountInfo of
        AddressAccount address ->
            address

        AddressContract address ->
            address

        AddressUnknown ->
            ""


accountInfoDecoder : D.Decoder AccountInfo
accountInfoDecoder =
    D.succeed
        (\tipe address ->
            case tipe of
                "AddressAccount" ->
                    AddressAccount address

                "AddressContract" ->
                    AddressContract address

                _ ->
                    AddressUnknown
        )
        |> required "type" D.string
        |> required "address" D.string


type alias EventBakerRemoved =
    { tag : String
    , bakerId : Int
    }


eventBakerRemovedDecoder : D.Decoder EventBakerRemoved
eventBakerRemovedDecoder =
    D.succeed EventBakerRemoved
        |> required "tag" (expectedTag "BakerRemoved")
        |> required "contents" D.int


type alias EventBakerAccountUpdated =
    { tag : String
    , bakerId : Int
    , newAccount : String
    }


eventBakerAccountUpdatedDecoder : D.Decoder EventBakerAccountUpdated
eventBakerAccountUpdatedDecoder =
    D.succeed EventBakerAccountUpdated
        |> required "tag" (expectedTag "BakerAccountUpdated")
        |> required "bakerId" D.int
        |> required "newAccount" D.string


type alias EventBakerKeyUpdated =
    { tag : String
    , bakerId : Int
    , newKey : String
    }


eventBakerKeyUpdatedDecoder : D.Decoder EventBakerKeyUpdated
eventBakerKeyUpdatedDecoder =
    D.succeed EventBakerKeyUpdated
        |> required "tag" (expectedTag "BakerKeyUpdated")
        |> required "bakerId" D.int
        |> required "newKey" D.string


type alias EventBakerElectionKeyUpdated =
    { tag : String
    , bakerId : Int
    , newKey : String
    }


eventBakerElectionKeyUpdatedDecoder : D.Decoder EventBakerElectionKeyUpdated
eventBakerElectionKeyUpdatedDecoder =
    D.succeed EventBakerElectionKeyUpdated
        |> required "tag" (expectedTag "BakerElectionKeyUpdated")
        |> required "bakerId" D.int
        |> required "newKey" D.string



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
    , amount : Amount
    , address : ContractAddress
    , name : Int
    , ref : String
    }


eventContractInitializedDecoder : D.Decoder EventContractInitialized
eventContractInitializedDecoder =
    D.succeed EventContractInitialized
        |> required "tag" (expectedTag "ContractInitialized")
        |> required "amount" decodeAmount
        |> required "address" contractAddressDecoder
        |> required "name" D.int
        |> required "ref" D.string


type alias EventContractMessage =
    { tag : String
    , amount : Amount
    , address : ContractAddress
    , message : String
    }


eventContractMessageDecoder : D.Decoder EventContractMessage
eventContractMessageDecoder =
    D.succeed EventContractMessage
        |> required "tag" (expectedTag "Updated")
        |> required "amount" decodeAmount
        |> required "address" contractAddressDecoder
        |> required "message" D.string


type alias ContractAddress =
    { index : Int
    , subindex : Int
    }


contractAddressDecoder : D.Decoder ContractAddress
contractAddressDecoder =
    D.succeed ContractAddress
        |> required "index" D.int
        |> required "subindex" D.int



-- Delegation


type alias EventStakeDelegated =
    { tag : String
    , account : String
    , baker : Int
    }


eventStakeDelegatedDecoder : D.Decoder EventStakeDelegated
eventStakeDelegatedDecoder =
    D.succeed EventStakeDelegated
        |> required "tag" (expectedTag "StakeDelegated")
        |> required "account" D.string
        |> required "baker" D.int


type alias EventStakeUndelegated =
    { tag : String
    , account : String
    , baker : Maybe Int
    }


eventStakeUndelegatedDecoder : D.Decoder EventStakeUndelegated
eventStakeUndelegatedDecoder =
    D.succeed EventStakeUndelegated
        |> required "tag" (expectedTag "StakeUndelegated")
        |> required "account" D.string
        |> required "baker" (D.nullable D.int)



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
