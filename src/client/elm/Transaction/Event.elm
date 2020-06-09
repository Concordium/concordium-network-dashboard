module Transaction.Event exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E



-- https://gitlab.com/Concordium/consensus/globalstate-types/-/blob/master/src/Concordium/Types/Execution.hs#L353


type TransactionEvent
    = TransactionEventTransfer EventTransfer
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


transactionEventsEncoder : TransactionEvent -> E.Value
transactionEventsEncoder item =
    case item of
        TransactionEventTransfer details ->
            eventTransferEncoder details

        -- Accounts
        TransactionEventAccountCreated details ->
            eventAccountCreatedEncoder details

        TransactionEventCredentialDeployed details ->
            eventCredentialDeployedEncoder details

        TransactionEventAccountEncryptionKeyDeployed details ->
            eventAccountEncryptionKeyDeployedEncoder details

        -- Baking
        TransactionEventBakerAdded details ->
            eventBakerAddedEncoder details

        TransactionEventBakerRemoved details ->
            eventBakerRemovedEncoder details

        TransactionEventBakerAccountUpdated details ->
            eventBakerAccountUpdatedEncoder details

        TransactionEventBakerKeyUpdated details ->
            eventBakerKeyUpdatedEncoder details

        TransactionEventBakerElectionKeyUpdated details ->
            eventBakerElectionKeyUpdatedEncoder details

        -- Contracts
        TransactionEventModuleDeployed details ->
            eventModuleDeployedEncoder details

        TransactionEventContractInitialized details ->
            eventContractInitializedEncoder details

        TransactionEventContractMessage details ->
            eventContractMessageEncoder details

        -- Delegation
        TransactionEventStakeDelegated details ->
            eventStakeDelegatedEncoder details

        TransactionEventStakeUndelegated details ->
            eventStakeUndelegatedEncoder details

        -- Core
        TransactionEventElectionDifficultyUpdated details ->
            eventElectionDifficultyUpdatedEncoder details

        -- Errors
        TransactionEventRejected details ->
            eventRejectedEncoder details


type alias EventTransfer =
    { amount : Int
    , tag : String
    , to : AccountInfo
    , from : AccountInfo
    }


eventTransferEncoder : EventTransfer -> E.Value
eventTransferEncoder item =
    E.object
        [ ( "amount", E.int item.amount )
        , ( "tag", E.string item.tag )
        , ( "to", accountInfoEncoder item.to )
        , ( "from", accountInfoEncoder item.from )
        ]


eventTransferDecoder : D.Decoder EventTransfer
eventTransferDecoder =
    D.succeed EventTransfer
        |> required "amount" D.int
        |> required "tag" (expectedTag "Transferred")
        |> required "to" accountInfoDecoder
        |> required "from" accountInfoDecoder



-- Accounts


type alias EventAccountCreated =
    { tag : String
    , account : String
    }


eventAccountCreatedEncoder : EventAccountCreated -> E.Value
eventAccountCreatedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "account", E.string item.account )
        ]


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


eventCredentialDeployedEncoder : EventCredentialDeployed -> E.Value
eventCredentialDeployedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "regid", E.string item.regid )
        , ( "account", E.string item.account )
        ]


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


eventAccountEncryptionKeyDeployedEncoder : EventAccountEncryptionKeyDeployed -> E.Value
eventAccountEncryptionKeyDeployedEncoder item =
    E.object
        [ ( "key", E.string item.key )
        , ( "account", E.string item.account )
        ]


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


eventBakerAddedEncoder : EventBakerAdded -> E.Value
eventBakerAddedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "contents", E.int item.bakerId )
        ]


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


accountInfoEncoder : AccountInfo -> E.Value
accountInfoEncoder item =
    case item of
        AddressAccount address ->
            E.object
                [ ( "type", E.string "AddressAccount" )
                , ( "address", E.string address )
                ]

        AddressContract address ->
            E.object
                [ ( "type", E.string "AddressContract" )
                , ( "address", E.string address )
                ]

        AddressUnknown ->
            E.null


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


eventBakerRemovedEncoder : EventBakerRemoved -> E.Value
eventBakerRemovedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "contents", E.int item.bakerId )
        ]


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


eventBakerAccountUpdatedEncoder : EventBakerAccountUpdated -> E.Value
eventBakerAccountUpdatedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "bakerId", E.int item.bakerId )
        , ( "newAccount", E.string item.newAccount )
        ]


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


eventBakerKeyUpdatedEncoder : EventBakerKeyUpdated -> E.Value
eventBakerKeyUpdatedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "bakerId", E.int item.bakerId )
        , ( "newKey", E.string item.newKey )
        ]


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


eventBakerElectionKeyUpdatedEncoder : EventBakerElectionKeyUpdated -> E.Value
eventBakerElectionKeyUpdatedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "bakerId", E.int item.bakerId )
        , ( "newKey", E.string item.newKey )
        ]


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


eventModuleDeployedEncoder : EventModuleDeployed -> E.Value
eventModuleDeployedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "contents", E.string item.contents )
        ]


eventModuleDeployedDecoder : D.Decoder EventModuleDeployed
eventModuleDeployedDecoder =
    D.succeed EventModuleDeployed
        |> required "tag" (expectedTag "ModuleDeployed")
        |> required "contents" D.string


type alias EventContractInitialized =
    { tag : String
    , amount : Int
    , address : ContractAddress
    , name : Int
    , ref : String
    }


eventContractInitializedEncoder : EventContractInitialized -> E.Value
eventContractInitializedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "amount", E.int item.amount )
        , ( "address", contractAddressEncoder item.address )
        , ( "name", E.int item.name )
        , ( "ref", E.string item.ref )
        ]


eventContractInitializedDecoder : D.Decoder EventContractInitialized
eventContractInitializedDecoder =
    D.succeed EventContractInitialized
        |> required "tag" (expectedTag "ContractInitialized")
        |> required "amount" D.int
        |> required "address" contractAddressDecoder
        |> required "name" D.int
        |> required "ref" D.string


type alias EventContractMessage =
    { tag : String
    , amount : Int
    , address : ContractAddress
    , message : String
    }


eventContractMessageEncoder : EventContractMessage -> E.Value
eventContractMessageEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "amount", E.int item.amount )
        , ( "address", contractAddressEncoder item.address )
        , ( "message", E.string item.message )
        ]


eventContractMessageDecoder : D.Decoder EventContractMessage
eventContractMessageDecoder =
    D.succeed EventContractMessage
        |> required "tag" (expectedTag "Updated")
        |> required "amount" D.int
        |> required "address" contractAddressDecoder
        |> required "message" D.string


type alias ContractAddress =
    { index : Int
    , subindex : Int
    }


contractAddressEncoder : ContractAddress -> E.Value
contractAddressEncoder item =
    E.object
        [ ( "index", E.int item.index )
        , ( "subindex", E.int item.subindex )
        ]


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


eventStakeDelegatedEncoder : EventStakeDelegated -> E.Value
eventStakeDelegatedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "account", E.string item.account )
        , ( "baker", E.int item.baker )
        ]


eventStakeDelegatedDecoder : D.Decoder EventStakeDelegated
eventStakeDelegatedDecoder =
    D.succeed EventStakeDelegated
        |> required "tag" (expectedTag "StakeDelegated")
        |> required "account" D.string
        |> required "baker" D.int


type alias EventStakeUndelegated =
    { tag : String
    , account : String
    , baker : Int
    }


eventStakeUndelegatedEncoder : EventStakeUndelegated -> E.Value
eventStakeUndelegatedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "account", E.string item.account )
        , ( "baker", E.int item.baker )
        ]


eventStakeUndelegatedDecoder : D.Decoder EventStakeUndelegated
eventStakeUndelegatedDecoder =
    D.succeed EventStakeUndelegated
        |> required "tag" (expectedTag "StakeUndelegated")
        |> required "account" D.string
        |> required "baker" D.int



-- Core


type alias EventElectionDifficultyUpdated =
    { tag : String
    , difficulty : Int
    }


eventElectionDifficultyUpdatedEncoder : EventElectionDifficultyUpdated -> E.Value
eventElectionDifficultyUpdatedEncoder item =
    E.object
        [ ( "tag", E.string item.tag )
        , ( "difficulty", E.int item.difficulty )
        ]


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


eventRejectedEncoder : EventRejected -> E.Value
eventRejectedEncoder item =
    E.object
        [ ( "transactionType", E.string item.transactionType )
        , ( "reason", E.string item.reason )
        , ( "hash", E.string item.hash )
        ]


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
