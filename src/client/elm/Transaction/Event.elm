module Transaction.Event exposing (..)

import Bytes as B
import Cbor
import Cbor.Decode
import Cbor.Encode
import Hex.Convert
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
    | TransactionEventTransferMemo EventTransferMemo
      -- Encrypted Transfers
    | TransactionEventNewEncryptedAmount EventNewEncryptedAmount
    | TransactionEventEncryptedAmountsRemoved EventEncryptedAmountsRemoved
      -- Accounts
    | TransactionEventAccountCreated EventAccountCreated
    | TransactionEventCredentialDeployed EventCredentialDeployed
    | TransactionEventCredentialKeysUpdated EventCredentialKeysUpdated
    | TransactionEventCredentialsUpdated EventCredentialsUpdated
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
    | TransactionEventDataRegistered EventDataRegistered



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


{-| Transaction metadata which is parsed as CBOR, if this fails it fallbacks to the raw hex string.
-}
type Memo
    = MemoString String
    | MemoInt Int
    | MemoRaw String


type alias EventTransferMemo =
    { memo : Memo
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


type alias EventCredentialKeysUpdated =
    { credId : String
    }


type alias EventCredentialsUpdated =
    { account : T.AccountAddress
    , newCredIds : List String
    , removedCredIds : List String
    , newThreshold : Int
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
    , receiveName : T.ReceiveName
    , events : List T.ContractEvent
    }


type alias EventDataRegistered =
    { data : String
    }



-- Core


type alias EventUpdateEnqueued =
    { effectiveTime : Posix
    , payload : UpdatePayload
    }


type FoundationAccountRepresentation
    = Index Int
    | Address T.AccountAddress


type UpdatePayload
    = MintDistributionPayload MintDistribution
    | TransactionFeeDistributionPayload TransactionFeeDistribution
    | GasRewardsPayload GasRewards
    | ElectionDifficultyPayload Float
    | EuroPerEnergyPayload Relation
    | MicroGtuPerEuroPayload Relation
    | FoundationAccountPayload FoundationAccountRepresentation
    | RootKeysUpdatePayload HigherLevelKeys
    | Level1KeysUpdatePayload HigherLevelKeys
    | Level2KeysUpdatePayload Authorizations
    | ProtocolUpdatePayload ProtocolUpdate
    | BakerStakeThresholdPayload T.Amount
    | AddAnonymityRevokerPayload AnonymityRevokerInfo
    | AddIdentityProviderPayload IdentityProviderInfo


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


type alias UpdateKeysCollection =
    { rootKeys : HigherLevelKeys
    , level1Keys : HigherLevelKeys
    , level2Keys : Authorizations
    }


type alias HigherLevelKeys =
    { threshold : Int
    , keys : List AuthorizationKey
    }


type alias Authorizations =
    { mintDistribution : Authorization
    , transactionFeeDistribution : Authorization
    , microGTUPerEuro : Authorization
    , euroPerEnergy : Authorization
    , electionDifficulty : Authorization
    , foundationAccount : Authorization
    , protocol : Authorization
    , paramGASRewards : Authorization
    , emergency : Authorization
    , bakerStakeThreshold : Authorization
    , keys : List AuthorizationKey
    }


type alias Authorization =
    { threshold : Int
    , authorizedKeys : List KeyIndex
    }


type alias KeyIndex =
    Int


type alias AuthorizationKey =
    { verifyKey : String
    , schemeId : String
    }


type alias ProtocolUpdate =
    { message : String
    , specificationURL : String
    , specificationHash : String
    , specificationAuxiliaryData : String
    }


type alias Description =
    { name : String
    , url : String
    , description : String
    }


{-| Identification number of an anonymity revoker or identity provider
-}
type Identity
    = ArIdentity Int
    | IpIdentity Int


{-| Information about anonymity revokers or identity providers
-}
type alias ArIpInfo =
    { identity : Identity
    , description : Description
    }


{-| Data for an anonymity revoker
-}
type AnonymityRevokerInfo
    = ArInfo ArIpInfo


{-| Data for an identity provider
-}
type IdentityProviderInfo
    = IpInfo ArIpInfo



-- Errors


type alias Relation =
    { denominator : Int
    , numerator : Int
    }


relationDecoder : D.Decoder Relation
relationDecoder =
    D.succeed Relation
        |> required "denominator" D.int
        |> required "numerator" D.int


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
                        relationDecoder |> D.map EuroPerEnergyPayload

                    "microGTUPerEuro" ->
                        relationDecoder |> D.map MicroGtuPerEuroPayload

                    "foundationAccount" ->
                        foundationAccountRepresentationDecoder |> D.map FoundationAccountPayload

                    "root" ->
                        keyUpdateDecoder

                    "level1" ->
                        keyUpdateDecoder

                    "protocol" ->
                        protocolUpdateDecoder |> D.map ProtocolUpdatePayload

                    "bakerStakeThreshold" ->
                        T.decodeAmount |> D.map BakerStakeThresholdPayload

                    "addAnonymityRevoker" ->
                        arDecoder |> D.map AddAnonymityRevokerPayload

                    "addIdentityProvider" ->
                        ipDecoder |> D.map AddIdentityProviderPayload

                    _ ->
                        D.fail "Unknown update type"
    in
    D.field "updateType" D.string |> D.andThen decode


foundationAccountRepresentationDecoder : D.Decoder FoundationAccountRepresentation
foundationAccountRepresentationDecoder =
    D.oneOf
        [ D.map Address T.accountAddressDecoder
        , D.map Index D.int
        ]


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


arDecoder : D.Decoder AnonymityRevokerInfo
arDecoder =
    let
        arIp =
            D.succeed ArIpInfo
                |> required "arIdentity" arIdentityDecoder
                |> required "arDescription" descriptionDecoder
    in
    D.map ArInfo arIp


arIdentityDecoder : D.Decoder Identity
arIdentityDecoder =
    D.map ArIdentity D.int


descriptionDecoder : D.Decoder Description
descriptionDecoder =
    D.succeed Description
        |> required "name" D.string
        |> required "url" D.string
        |> required "description" D.string


ipDecoder : D.Decoder IdentityProviderInfo
ipDecoder =
    let
        arIp =
            D.succeed ArIpInfo
                |> required "ipIdentity" ipIdentityDecoder
                |> required "ipDescription" descriptionDecoder
    in
    D.map IpInfo arIp


ipIdentityDecoder : D.Decoder Identity
ipIdentityDecoder =
    D.map IpIdentity D.int


updateKeysCollectionDecoder : D.Decoder UpdateKeysCollection
updateKeysCollectionDecoder =
    D.succeed UpdateKeysCollection
        |> required "rootKeys" higherLevelKeysDecoder
        |> required "level1Keys" higherLevelKeysDecoder
        |> required "level2Keys" authorizationsDecoder


authorizationsDecoder : D.Decoder Authorizations
authorizationsDecoder =
    D.succeed Authorizations
        |> required "mintDistribution" authorizationDecoder
        |> required "transactionFeeDistribution" authorizationDecoder
        |> required "microGTUPerEuro" authorizationDecoder
        |> required "euroPerEnergy" authorizationDecoder
        |> required "electionDifficulty" authorizationDecoder
        |> required "foundationAccount" authorizationDecoder
        |> required "protocol" authorizationDecoder
        |> required "paramGASRewards" authorizationDecoder
        |> required "emergency" authorizationDecoder
        |> required "bakerStakeThreshold" authorizationDecoder
        |> required "keys" (D.list authorizationKeyDecoder)


higherLevelKeysDecoder : D.Decoder HigherLevelKeys
higherLevelKeysDecoder =
    D.succeed HigherLevelKeys
        |> required "threshold" D.int
        |> required "keys" (D.list authorizationKeyDecoder)


keyUpdateDecoder : D.Decoder UpdatePayload
keyUpdateDecoder =
    let
        decode keyType =
            case keyType of
                "rootKeysUpdate" ->
                    D.succeed RootKeysUpdatePayload
                        |> required "updatePayload" higherLevelKeysDecoder

                "level1KeysUpdate" ->
                    D.succeed Level1KeysUpdatePayload
                        |> required "updatePayload" higherLevelKeysDecoder

                "level2KeysUpdate" ->
                    D.succeed Level2KeysUpdatePayload
                        |> required "updatePayload" authorizationsDecoder

                _ ->
                    D.fail <| "Unknown key update type: " ++ keyType
    in
    D.field "typeOfUpdate" D.string |> D.andThen decode


authorizationDecoder : D.Decoder Authorization
authorizationDecoder =
    D.succeed Authorization
        |> required "threshold" D.int
        |> required "authorizedKeys" (D.list D.int)


authorizationKeyDecoder : D.Decoder AuthorizationKey
authorizationKeyDecoder =
    D.succeed AuthorizationKey
        |> required "verifyKey" D.string
        |> required "schemeId" D.string


protocolUpdateDecoder : D.Decoder ProtocolUpdate
protocolUpdateDecoder =
    D.succeed ProtocolUpdate
        |> required "message" D.string
        |> required "specificationURL" D.string
        |> required "specificationHash" D.string
        |> required "specificationAuxiliaryData" D.string


{-| Convert Maybe a to a Decoder which fails if the Maybe is Nothing.
It also takes a string for the message to fail with.
-}
decoderFromMaybe : String -> Maybe a -> D.Decoder a
decoderFromMaybe error m =
    case m of
        Just n ->
            D.succeed n

        Nothing ->
            D.fail error


{-| Decode a JSON string encoded in hex to bytes
-}
hexDecoder : D.Decoder B.Bytes
hexDecoder =
    D.string |> D.andThen (\str -> decoderFromMaybe "Failed to decode Hex string" (Hex.Convert.toBytes str))


{-| Wrap a CBOR decoder in a JSON Decoder.
It also takes a corresponding CBOR encoder which encodes the bytes again to compare with the input bytes to ensure every byte is consumed.
This is possibly not the best solution, but the only one to have worked so far.
-}
cborDecoder : Cbor.Decode.Decoder a -> (a -> Cbor.Encode.Encoder) -> B.Bytes -> D.Decoder a
cborDecoder decoder encoder bytes =
    decoderFromMaybe "Failed to decode Cbor" (Cbor.Decode.decode decoder bytes)
        |> D.andThen
            (\value ->
                let
                    valueBytes =
                        Cbor.Encode.encode (encoder value)
                in
                -- Compare the number of bytes, instead of bytes directly, since Elm uses referential equality
                -- and referential equality would always result in false in this case.
                if B.width valueBytes == B.width bytes then
                    D.succeed value

                else
                    D.fail "Bytes remaining from decoding CBOR"
            )


{-| Memo bytes are encoded as Hex in a JSON string. We try to decode the hex and
then try to decode the bytes as a CBOR string or a CBOR int,
if this fails we fallback to the raw JSON string
-}
memoDecoder : D.Decoder Memo
memoDecoder =
    D.oneOf
        [ hexDecoder
            |> D.andThen
                (\memoBytes ->
                    D.oneOf
                        [ cborDecoder Cbor.Decode.string Cbor.Encode.string memoBytes |> D.map MemoString
                        , cborDecoder Cbor.Decode.int Cbor.Encode.int memoBytes |> D.map MemoInt
                        ]
                )
        , D.string |> D.map MemoRaw
        ]


{-| Convert memo to a string suited for viewing
-}
memoToString : Memo -> String
memoToString memo =
    case memo of
        MemoString str ->
            "\"" ++ str ++ "\""

        MemoInt int ->
            String.fromInt int

        MemoRaw raw ->
            raw


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

                "TransferMemo" ->
                    D.succeed EventTransferMemo
                        |> required "memo" memoDecoder
                        |> D.map TransactionEventTransferMemo

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

                "CredentialKeysUpdated" ->
                    D.succeed EventCredentialKeysUpdated
                        |> required "credId" D.string
                        |> D.map TransactionEventCredentialKeysUpdated

                "CredentialsUpdated" ->
                    D.succeed EventCredentialsUpdated
                        |> required "account" T.accountAddressDecoder
                        |> required "newCredIds" (D.list D.string)
                        |> required "removedCredIds" (D.list D.string)
                        |> required "newThreshold" D.int
                        |> D.map TransactionEventCredentialsUpdated

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
                        |> required "initName" T.contractInitNameDecoder
                        |> required "events" (D.list T.contractEventDecoder)
                        |> D.map TransactionEventContractInitialized

                "Updated" ->
                    D.succeed EventContractUpdated
                        |> required "address" T.contractAddressDecoder
                        |> required "instigator" T.addressDecoder
                        |> required "amount" T.decodeAmount
                        |> required "message" D.string
                        |> required "receiveName" T.contractReceiveNameDecoder
                        |> required "events" (D.list T.contractEventDecoder)
                        |> D.map TransactionEventContractUpdated

                -- Core
                "UpdateEnqueued" ->
                    D.succeed EventUpdateEnqueued
                        |> required "effectiveTime" (D.map (\seconds -> Time.millisToPosix (seconds * 1000)) D.int)
                        |> required "payload" updatePayloadDecoder
                        |> D.map TransactionEventUpdateEnqueued

                "DataRegistered" ->
                    D.succeed EventDataRegistered
                        |> required "data" D.string
                        |> D.map TransactionEventDataRegistered

                -- Errors
                _ ->
                    D.fail <| "Unknown event tag: " ++ tag
    in
    D.field "tag" D.string |> D.andThen decode
