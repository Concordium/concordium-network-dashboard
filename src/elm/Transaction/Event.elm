module Transaction.Event exposing (..)

import Bytes as B
import Cbor
import Cbor.Decode
import Cbor.Encode
import Hex.Convert
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
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
    | TransactionEventBakerSetOpenStatus EventBakerSetOpenStatus
    | TransactionEventBakerSetMetadataURL EventBakerSetMetadataURL
    | TransactionEventBakerSetTransactionFeeCommission EventBakerSetTransactionFeeCommission
    | TransactionEventBakerSetBakingRewardCommission EventBakerSetBakingRewardCommission
    | TransactionEventBakerSetFinalizationRewardCommission EventBakerSetFinalizationRewardCommission
      -- Contracts
    | TransactionEventModuleDeployed EventModuleDeployed
    | TransactionEventContractInitialized EventContractInitialized
    | TransactionEventContractUpdated EventContractUpdated
    | TransactionEventContractInterrupted EventContractInterrupted
    | TransactionEventContractResumed EventContractResumed
    | TransactionEventContractUpgraded EventContractUpgraded
      -- Delegation
    | TransactionEventDelegationStakeIncreased EventDelegationStakeIncreased
    | TransactionEventDelegationStakeDecreased EventDelegationStakeDecreased
    | TransactionEventDelegationSetRestakeEarnings EventDelegationSetRestakeEarnings
    | TransactionEventDelegationSetDelegationTarget EventDelegationSetDelegationTarget
    | TransactionEventDelegationAdded EventDelegationAdded
    | TransactionEventDelegationRemoved EventDelegationRemoved
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


{-| Arbitrary bytes, which could be encoded in CBOR otherwise fallback to a hex string.
-}
type ArbitraryBytes
    = CborString String
    | CborInt Int
    | RawHex String


type alias EventTransferMemo =
    { memo : ArbitraryBytes
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


type alias EventBakerSetOpenStatus =
    { bakerId : Int
    , account : T.AccountAddress
    , openStatus : String
    }


type alias EventBakerSetMetadataURL =
    { bakerId : Int
    , account : T.AccountAddress
    , metadataURL : String
    }


type alias EventBakerSetTransactionFeeCommission =
    { bakerId : Int
    , account : T.AccountAddress
    , transactionFeeCommission : Float
    }


type alias EventBakerSetBakingRewardCommission =
    { bakerId : Int
    , account : T.AccountAddress
    , bakingRewardCommission : Float
    }


type alias EventBakerSetFinalizationRewardCommission =
    { bakerId : Int
    , account : T.AccountAddress
    , finalizationRewardCommission : Float
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


type alias EventContractInterrupted =
    { address : T.ContractAddress
    , events : List T.ContractEvent
    }


type alias EventContractResumed =
    { address : T.ContractAddress
    , success : Bool
    }


type alias EventContractUpgraded =
    { address : T.ContractAddress
    , from : T.ModuleRef
    , to : T.ModuleRef
    }


type alias EventDataRegistered =
    { data : ArbitraryBytes
    }



-- Delegation


type alias EventDelegationStakeIncreased =
    { delegatorId : Int
    , account : T.AccountAddress
    , newStake : T.Amount
    }


type alias EventDelegationStakeDecreased =
    { delegatorId : Int
    , account : T.AccountAddress
    , newStake : T.Amount
    }


type alias EventDelegationSetRestakeEarnings =
    { delegatorId : Int
    , account : T.AccountAddress
    , restakeEarnings : Bool
    }


type alias EventDelegationSetDelegationTarget =
    { delegatorId : Int
    , account : T.AccountAddress
    , delegationTarget : Maybe Int
    }


type alias EventDelegationAdded =
    { delegatorId : Int
    , account : T.AccountAddress
    }


type alias EventDelegationRemoved =
    { delegatorId : Int
    , account : T.AccountAddress
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
    | MicroCCDPerEuroPayload Relation
    | FoundationAccountPayload FoundationAccountRepresentation
    | RootKeysUpdatePayload HigherLevelKeys
    | Level1KeysUpdatePayload HigherLevelKeys
    | Level2KeysUpdatePayload Authorizations
    | ProtocolUpdatePayload ProtocolUpdate
    | AddAnonymityRevokerPayload AnonymityRevokerInfo
    | AddIdentityProviderPayload IdentityProviderInfo
    | PoolParametersPayload PoolParameters
    | CooldownParametersPayload CooldownParameters
    | TimeParametersPayload TimeParameters


type MintDistribution
    = MDV0 MintDistributionV0
    | MDV1 MintDistributionV1


type alias MintDistributionV0 =
    { mintPerSlot : Float
    , bakingReward : Float
    , finalizationReward : Float
    }


type alias MintDistributionV1 =
    { bakingReward : Float
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
    , microCCDPerEuro : Authorization
    , euroPerEnergy : Authorization
    , electionDifficulty : Authorization
    , foundationAccount : Authorization
    , protocol : Authorization
    , paramGASRewards : Authorization
    , emergency : Authorization
    , bakerStakeThreshold : Authorization
    , poolParameters : Authorization
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


type PoolParameters
    = PPV0 PoolParametersV0
    | PPV1 PoolParametersV1


{-| 'bakerStakeThreshold' is a single pool parameter in V0
-}
type PoolParametersV0
    = PoolParametersV0 T.Amount


type alias PoolParametersV1 =
    { passiveFinalizationCommission : Float
    , passiveBakingCommission : Float
    , passiveTransactionCommission : Float
    , finalizationCommissionRange : Range Float
    , bakingCommissionRange : Range Float
    , transactionCommissionRange : Range Float
    , minimumEquityCapital : T.Amount
    , capitalBound : Float
    , leverageBound : Relation
    }


type CooldownParameters
    = CDPV0 CooldownParametersV0
    | CDPV1 CooldownParametersV1


{-| 'bakerCooldownEpochs' is a single cooldown parameter in V0
-}
type CooldownParametersV0
    = CooldownParametersV0 Int


type alias CooldownParametersV1 =
    { poolOwnerCooldown : Int
    , delegatorCooldown : Int
    }


type alias TimeParameters =
    { rewardPeriodLength : Int
    , mintPerDay : Float
    }



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


{-| A range that includes both endpoints.
-}
type alias Range a =
    { min : a
    , max : a
    }


rangeDecoder : D.Decoder a -> D.Decoder (Range a)
rangeDecoder dec =
    D.succeed Range
        |> required "min" dec
        |> required "max" dec


updatePayloadDecoder : D.Decoder UpdatePayload
updatePayloadDecoder =
    let
        decode updateType =
            D.field "update" <|
                case updateType of
                    "mintDistributionV0" ->
                        mintDistributionV0Decoder
                            |> D.map MintDistributionPayload

                    "mintDistributionV1" ->
                        mintDistributionV1Decoder
                            |> D.map MintDistributionPayload

                    "transactionFeeDistribution" ->
                        transactionFeeDistributionDecoder |> D.map TransactionFeeDistributionPayload

                    "gasRewards" ->
                        gasRewardsDecoder |> D.map GasRewardsPayload

                    "electionDifficulty" ->
                        D.float |> D.map ElectionDifficultyPayload

                    "euroPerEnergy" ->
                        relationDecoder |> D.map EuroPerEnergyPayload

                    "microGTUPerEuro" ->
                        relationDecoder |> D.map MicroCCDPerEuroPayload

                    "foundationAccount" ->
                        foundationAccountRepresentationDecoder |> D.map FoundationAccountPayload

                    "root" ->
                        keyUpdateDecoder

                    "level1" ->
                        keyUpdateDecoder

                    "protocol" ->
                        protocolUpdateDecoder |> D.map ProtocolUpdatePayload

                    "addAnonymityRevoker" ->
                        arDecoder |> D.map AddAnonymityRevokerPayload

                    "addIdentityProvider" ->
                        ipDecoder |> D.map AddIdentityProviderPayload

                    "poolParametersV0" ->
                        poolParametersV0Decoder |> D.map PoolParametersPayload

                    "poolParametersV1" ->
                        poolParametersV1Decoder |> D.map PoolParametersPayload

                    "cooldownParametersV1" ->
                        cooldownParametersV1Decoder |> D.map CooldownParametersPayload

                    "timeParametersV1" ->
                        timeParametersDecoder |> D.map TimeParametersPayload

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


mintDistributionV0Decoder : D.Decoder MintDistribution
mintDistributionV0Decoder =
    D.succeed MintDistributionV0
        |> required "mintPerSlot" D.float
        |> required "bakingReward" D.float
        |> required "finalizationReward" D.float
        |> D.map MDV0


mintDistributionV1Decoder : D.Decoder MintDistribution
mintDistributionV1Decoder =
    D.succeed MintDistributionV1
        |> required "bakingReward" D.float
        |> required "finalizationReward" D.float
        |> D.map MDV1


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
        |> optional "bakerStakeThreshold" authorizationDecoder { threshold = 0, authorizedKeys = [] }
        |> optional "poolParameters" authorizationDecoder { threshold = 0, authorizedKeys = [] }
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


poolParametersV0Decoder : D.Decoder PoolParameters
poolParametersV0Decoder =
    D.succeed PoolParametersV0
        |> required "minimumThresholdForBaking" T.decodeAmount
        |> D.map PPV0


poolParametersV1Decoder : D.Decoder PoolParameters
poolParametersV1Decoder =
    D.succeed PoolParametersV1
        |> required "passiveFinalizationCommission" D.float
        |> required "passiveBakingCommission" D.float
        |> required "passiveTransactionCommission" D.float
        |> required "bakingCommissionRange" (rangeDecoder D.float)
        |> required "transactionCommissionRange" (rangeDecoder D.float)
        |> required "finalizationCommissionRange" (rangeDecoder D.float)
        |> required "minimumEquityCapital" T.decodeAmount
        |> required "capitalBound" D.float
        |> required "leverageBound" relationDecoder
        |> D.map PPV1


cooldownParametersV0Decoder : D.Decoder CooldownParameters
cooldownParametersV0Decoder =
    D.succeed CooldownParametersV0
        |> required "bakerCooldownEpochs" D.int
        |> D.map CDPV0


cooldownParametersV1Decoder : D.Decoder CooldownParameters
cooldownParametersV1Decoder =
    D.succeed CooldownParametersV1
        |> required "poolOwnerCooldown" D.int
        |> required "delegatorCooldown" D.int
        |> D.map CDPV1


timeParametersDecoder : D.Decoder TimeParameters
timeParametersDecoder =
    D.succeed TimeParameters
        |> required "rewardPeriodLength" D.int
        |> required "mintPerPayday" D.float


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


{-| Arbitrary bytes are encoded as Hex in a JSON string. We try to decode the hex and
then try to decode the bytes as a CBOR string or a CBOR int,
if this fails we fallback to the raw JSON string
-}
arbitraryBytesDecoder : D.Decoder ArbitraryBytes
arbitraryBytesDecoder =
    D.oneOf
        [ hexDecoder
            |> D.andThen
                (\bytes ->
                    D.oneOf
                        [ cborDecoder Cbor.Decode.string Cbor.Encode.string bytes |> D.map CborString
                        , cborDecoder Cbor.Decode.int Cbor.Encode.int bytes |> D.map CborInt
                        ]
                )
        , D.string |> D.map RawHex
        ]


{-| Convert ArbitraryBytes to a string suited for viewing
-}
arbitraryBytesToString : ArbitraryBytes -> String
arbitraryBytesToString arbitraryBytes =
    case arbitraryBytes of
        CborString str ->
            "decodes as the string \"" ++ str ++ "\""

        CborInt int ->
            "decodes as the integer " ++ String.fromInt int

        RawHex raw ->
            "cannot be decoded. The raw data is: " ++ raw


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
                        |> required "memo" arbitraryBytesDecoder
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

                "BakerSetOpenStatus" ->
                    D.succeed EventBakerSetOpenStatus
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "openStatus" D.string
                        |> D.map TransactionEventBakerSetOpenStatus

                "BakerSetMetadataURL" ->
                    D.succeed EventBakerSetMetadataURL
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "metadataURL" D.string
                        |> D.map TransactionEventBakerSetMetadataURL

                "BakerSetTransactionFeeCommission" ->
                    D.succeed EventBakerSetTransactionFeeCommission
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "transactionFeeCommission" D.float
                        |> D.map TransactionEventBakerSetTransactionFeeCommission

                "BakerSetBakingRewardCommission" ->
                    D.succeed EventBakerSetBakingRewardCommission
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "bakingRewardCommission" D.float
                        |> D.map TransactionEventBakerSetBakingRewardCommission

                "BakerSetFinalizationRewardCommission" ->
                    D.succeed EventBakerSetFinalizationRewardCommission
                        |> required "bakerId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "finalizationRewardCommission" D.float
                        |> D.map TransactionEventBakerSetFinalizationRewardCommission

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

                "Interrupted" ->
                    D.succeed EventContractInterrupted
                        |> required "address" T.contractAddressDecoder
                        |> required "events" (D.list T.contractEventDecoder)
                        |> D.map TransactionEventContractInterrupted

                "Resumed" ->
                    D.succeed EventContractResumed
                        |> required "address" T.contractAddressDecoder
                        |> required "success" D.bool
                        |> D.map TransactionEventContractResumed

                "Upgraded" ->
                    D.succeed EventContractUpgraded
                        |> required "address" T.contractAddressDecoder
                        |> required "from" D.string
                        |> required "to" D.string
                        |> D.map TransactionEventContractUpgraded

                -- Delegation
                "DelegationStakeIncreased" ->
                    D.succeed EventDelegationStakeIncreased
                        |> required "delegatorId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "newStake" T.decodeAmount
                        |> D.map TransactionEventDelegationStakeIncreased

                "DelegationStakeDecreased" ->
                    D.succeed EventDelegationStakeDecreased
                        |> required "delegatorId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "newStake" T.decodeAmount
                        |> D.map TransactionEventDelegationStakeDecreased

                "DelegationSetRestakeEarnings" ->
                    D.succeed EventDelegationSetRestakeEarnings
                        |> required "delegatorId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "restakeEarnings" D.bool
                        |> D.map TransactionEventDelegationSetRestakeEarnings

                "DelegationSetDelegationTarget" ->
                    D.succeed EventDelegationSetDelegationTarget
                        |> required "delegatorId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> required "delegationTarget" T.delegationTargetDecoder
                        |> D.map TransactionEventDelegationSetDelegationTarget

                "DelegationAdded" ->
                    D.succeed EventDelegationAdded
                        |> required "delegatorId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> D.map TransactionEventDelegationAdded

                "DelegationRemoved" ->
                    D.succeed EventDelegationRemoved
                        |> required "delegatorId" D.int
                        |> required "account" T.accountAddressDecoder
                        |> D.map TransactionEventDelegationRemoved

                -- Core
                "UpdateEnqueued" ->
                    D.succeed EventUpdateEnqueued
                        |> required "effectiveTime" (D.map (\seconds -> Time.millisToPosix (seconds * 1000)) D.int)
                        |> required "payload" updatePayloadDecoder
                        |> D.map TransactionEventUpdateEnqueued

                "DataRegistered" ->
                    D.succeed EventDataRegistered
                        |> required "data" arbitraryBytesDecoder
                        |> D.map TransactionEventDataRegistered

                -- Errors
                _ ->
                    D.fail <| "Unknown event tag: " ++ tag
    in
    D.field "tag" D.string |> D.andThen decode
