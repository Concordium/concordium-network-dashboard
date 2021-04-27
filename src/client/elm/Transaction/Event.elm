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


type UpdatePayload
    = MintDistributionPayload MintDistribution
    | TransactionFeeDistributionPayload TransactionFeeDistribution
    | GasRewardsPayload GasRewards
    | ElectionDifficultyPayload Float
    | EuroPerEnergyPayload Relation
    | MicroGtuPerEuroPayload Relation
    | FoundationAccountPayload T.AccountAddress
    | RootKeysUpdatePayload HigherLevelKeys
    | Level1KeysUpdatePayload HigherLevelKeys
    | Level2KeysUpdatePayload Authorizations
    | ProtocolUpdatePayload ProtocolUpdate
    | BakerStakeThresholdPayload T.Amount
    | UpdateAddAnonymityRevokerPayload AnonymityRevokerInfo
    | UpdateAddIdentityProviderPayload IdentityProviderInfo


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
    { name: String
    , url: String
    , description: String }


type alias ArIdentity =
    Int


type alias AnonymityRevokerInfo =
    { arIdentity: ArIdentity
    , arDescription: Description }


type alias IpIdentity =
    Int


type alias IdentityProviderInfo =
    { ipIdentity: IpIdentity
    , ipDescription: Description }


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
                        T.accountAddressDecoder |> D.map FoundationAccountPayload

                    "root" ->
                        keyUpdateDecoder

                    "level1" ->
                        keyUpdateDecoder

                    "protocol" ->
                        protocolUpdateDecoder |> D.map ProtocolUpdatePayload

                    "bakerStakeThreshold" ->
                        T.decodeAmount |> D.map BakerStakeThresholdPayload

                    "addAnonymityRevoker" ->
                        arDecoder |> D.map UpdateAddAnonymityRevokerPayload

                    "addIdentityProvider" ->
                        ipDecoder |> D.map UpdateAddIdentityProviderPayload

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


arDecoder : D.Decoder AnonymityRevokerInfo
arDecoder =
    D.succeed AnonymityRevokerInfo
        |> required "arIdentity" D.int
        |> required "arDescription" descriptionDecoder


descriptionDecoder : D.Decoder Description
descriptionDecoder =
    D.succeed Description
        |> required "name" D.string
        |> required "url" D.string
        |> required "description" D.string


ipDecoder : D.Decoder IdentityProviderInfo
ipDecoder =
    D.succeed IdentityProviderInfo
        |> required "ipIdentity" D.int
        |> required "ipDescription" descriptionDecoder


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
