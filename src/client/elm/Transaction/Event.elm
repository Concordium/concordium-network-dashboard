module Transaction.Event exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type TransactionEvent
    = TransactionEventTransfer EventTransfer
    | TransactionEventAccountCreated EventAccountCreated
    | TransactionEventCredentialDeployed EventCredentialDeployed
    | TransactionEventStakeDelegated EventStakeDelegated
    | TransactionEventBakerAdded EventBakerAdded


transactionEventsDecoder : D.Decoder TransactionEvent
transactionEventsDecoder =
    D.oneOf
        [ D.map TransactionEventTransfer eventTransferDecoder
        , D.map TransactionEventAccountCreated eventAccountCreatedDecoder
        , D.map TransactionEventCredentialDeployed eventCredentialDeployedDecoder
        , D.map TransactionEventStakeDelegated eventStakeDelegatedDecoder
        , D.map TransactionEventBakerAdded eventBakerAddedDecoder
        ]


type alias EventTransfer =
    { amount : Int
    , tag : String
    , to : AccountInfo
    , from : AccountInfo
    }


eventTransferDecoder : D.Decoder EventTransfer
eventTransferDecoder =
    D.succeed EventTransfer
        |> required "amount" D.int
        |> required "tag" D.string
        |> required "to" accountInfoDecoder
        |> required "from" accountInfoDecoder


type alias EventAccountCreated =
    { tag : String
    , account : String
    }


eventAccountCreatedDecoder : D.Decoder EventAccountCreated
eventAccountCreatedDecoder =
    D.succeed EventAccountCreated
        |> required "tag" D.string
        |> required "contents" D.string


type alias EventCredentialDeployed =
    { tag : String
    , regid : String
    , account : String
    }


eventCredentialDeployedDecoder : D.Decoder EventCredentialDeployed
eventCredentialDeployedDecoder =
    D.succeed EventCredentialDeployed
        |> required "tag" D.string
        |> required "regid" D.string
        |> required "account" D.string


type alias EventStakeDelegated =
    { tag : String
    , account : String
    , baker : Int
    }


eventStakeDelegatedDecoder : D.Decoder EventStakeDelegated
eventStakeDelegatedDecoder =
    D.succeed EventStakeDelegated
        |> required "tag" D.string
        |> required "account" D.string
        |> required "baker" D.int


type alias EventBakerAdded =
    { tag : String
    , contents : Int
    }


eventBakerAddedDecoder : D.Decoder EventBakerAdded
eventBakerAddedDecoder =
    D.succeed EventBakerAdded
        |> required "tag" D.string
        |> required "contents" D.int


type AccountInfo
    = AddressAccount String
    | AddressContract String
    | AddressUnknown


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
