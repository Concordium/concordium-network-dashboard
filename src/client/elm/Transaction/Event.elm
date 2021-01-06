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



-- Transfers


type alias EventTransfer =
    { amount : T.Amount
    , to : T.AccountInfo
    , from : T.AccountInfo
    }


type alias EventEncryptedSelfAmountAdded =
    { account : String
    , amount : T.Amount
    }


type alias EventAmountAddedByDecryption =
    { account : String
    , amount : T.Amount
    }



-- Encrypted Transfers


type alias EventNewEncryptedAmount =
    { account : String }


type alias EventEncryptedAmountsRemoved =
    { account : String }



-- Accounts


type alias EventAccountCreated =
    { account : String }


type alias EventCredentialDeployed =
    { regid : String
    , account : String
    }


type alias EventAccountEncryptionKeyDeployed =
    { key : String
    , account : String
    }



-- Baking


type alias EventBakerAdded =
    { bakerId : Int
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
    { amount : T.Amount
    , address : T.ContractAddress
    , name : Int
    , ref : String
    }


type alias EventContractMessage =
    { amount : T.Amount
    , address : T.ContractAddress
    , message : String
    }



-- Core


type alias EventElectionDifficultyUpdated =
    { difficulty : Int }



-- Errors


type alias EventRejected =
    -- @TODO swap to camel case
    { transactionType : String
    , reason : String
    , hash : String
    }


transactionEventsDecoder : D.Decoder TransactionEvent
transactionEventsDecoder =
    let
        decode tag =
            case tag of
                "Transferred" ->
                    D.succeed EventTransfer
                        |> required "amount" T.decodeAmount
                        |> required "to" T.accountInfoDecoder
                        |> required "from" T.accountInfoDecoder
                        |> D.map TransactionEventTransfer

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

                -- Baking
                "BakerAdded" ->
                    D.succeed EventBakerAdded
                        |> required "bakerId" D.int
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
                        |> required "amount" T.decodeAmount
                        |> required "address" T.contractAddressDecoder
                        |> required "name" D.int
                        |> required "ref" D.string
                        |> D.map TransactionEventContractInitialized

                "Updated" ->
                    D.succeed EventContractMessage
                        |> required "amount" T.decodeAmount
                        |> required "address" T.contractAddressDecoder
                        |> required "message" D.string
                        |> D.map TransactionEventContractMessage

                -- Core
                "ElectionDifficultyUpdated" ->
                    D.succeed EventElectionDifficultyUpdated
                        |> required "difficulty" D.int
                        |> D.map TransactionEventElectionDifficultyUpdated

                -- Errors
                _ ->
                    D.succeed EventRejected
                        |> required "transactionType" D.string
                        |> required "reason" D.string
                        |> required "hash" D.string
                        |> D.map TransactionEventRejected
    in
    D.field "tag" D.string |> D.andThen decode
