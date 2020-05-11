module Transaction.Event exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E


type TransactionEvent
    = TransactionEventTransfer EventTransfer
    | TransactionEventAccountCreated EventAccountCreated
    | TransactionEventCredentialDeployed EventCredentialDeployed
    | TransactionEventStakeDelegated EventStakeDelegated
    | TransactionEventBakerAdded EventBakerAdded
    | TransactionEventModuleDeployed EventModuleDeployed
    | TransactionEventContractInitialized EventContractInitialized
    | TransactionEventContractMessage EventContractMessage


transactionEventsDecoder : D.Decoder TransactionEvent
transactionEventsDecoder =
    D.oneOf
        [ D.map TransactionEventTransfer eventTransferDecoder
        , D.map TransactionEventAccountCreated eventAccountCreatedDecoder
        , D.map TransactionEventCredentialDeployed eventCredentialDeployedDecoder
        , D.map TransactionEventStakeDelegated eventStakeDelegatedDecoder
        , D.map TransactionEventBakerAdded eventBakerAddedDecoder
        , D.map TransactionEventModuleDeployed eventModuleDeployedDecoder
        , D.map TransactionEventContractInitialized eventContractInitializedDecoder
        , D.map TransactionEventContractMessage eventContractMessageDecoder
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
    , amount : Int
    , address : ContractAddress
    , name : Int
    , ref : String
    }


eventContractInitializedDecoder : D.Decoder EventContractInitialized
eventContractInitializedDecoder =
    D.succeed EventContractInitialized
        |> required "tag" D.string
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


eventContractMessageDecoder : D.Decoder EventContractMessage
eventContractMessageDecoder =
    D.succeed EventContractMessage
        |> required "tag" D.string
        |> required "amount" D.int
        |> required "address" contractAddressDecoder
        |> required "message" D.string


type alias ContractAddress =
    { index : Int, subindex : Int }


contractAddressDecoder : D.Decoder ContractAddress
contractAddressDecoder =
    D.succeed ContractAddress
        |> required "index" D.int
        |> required "subindex" D.int


contractAddressEncoder : ContractAddress -> E.Value
contractAddressEncoder item =
    E.object
        [ ( "index", E.int item.index )
        , ( "subindex", E.int item.subindex )
        ]


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
