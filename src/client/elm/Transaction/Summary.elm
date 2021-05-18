module Transaction.Summary exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (custom, optional, required)
import Transaction.Event exposing (..)
import Types as T


type alias TransactionSummary =
    { hash : String
    , sender : Maybe T.AccountAddress
    , cost : T.Amount
    , result : TransactionResult
    , energyCost : T.Energy
    , tipe : TransactionSummaryType
    }


{-| Type of block transaction summary

Must be in sync with `TransactionSummaryType` found in
<https://github.com/Concordium/concordium-base/blob/main/haskell-src/Concordium/Types/Execution.hs>

-}
type TransactionSummaryType
    = AccountTransaction AccountTransactionType -- Contains a constructor `Malformed` to represent `Nothing` in the corresponding type in haskell
    | CredentialDeploymentTransaction CredentialType
    | UpdateTransaction UpdateType


{-| Reason for transaction rejected

Must be in sync with the constructor names of `Payload` found in
<https://github.com/Concordium/concordium-base/blob/main/haskell-src/Concordium/Types/Execution.hs>

Also contains a `Malformed` constructor to represent Nothing.

-}
type AccountTransactionType
    = DeployModule
    | InitContract
    | Update
    | Transfer
    | AddBaker
    | RemoveBaker
    | UpdateBakerStake
    | UpdateBakerRestakeEarnings
    | UpdateBakerKeys
    | UpdateCredentialKeys
    | EncryptedAmountTransfer
    | TransferToEncrypted
    | TransferToPublic
    | TransferWithSchedule
    | UpdateCredentials
    | RegisterData
    | Malformed


type CredentialType
    = Initial
    | Normal


{-| Must be in sync with `UpdateType` found in
<https://github.com/Concordium/concordium-base/blob/main/haskell-src/Concordium/Types/Updates.hs>
-}
type UpdateType
    = UpdateRootKeysWithRootKeys
      -- ^Update the root keys using the root keys
    | UpdateLevel1KeysWithRootKeys
      -- ^Update the level 1 keys using the root keys
    | UpdateLevel2KeysWithRootKeys
      -- ^Update the level 2 keys using the root keys
    | UpdateLevel1KeysWithLevel1Keys
      -- ^Update the level 1 keys using the level 1 keys
    | UpdateLevel2KeysWithLevel1Keys
      -- ^Update the level 2 keys using the level 1 keys
    | UpdateProtocol
      -- ^Update the chain protocol
    | UpdateElectionDifficulty
      -- ^Update the election difficulty
    | UpdateEuroPerEnergy
      -- ^Update the euro per energy exchange rate
    | UpdateMicroGTUPerEuro
      -- ^Update the microGTU per euro exchange rate
    | UpdateFoundationAccount
      -- ^Update the address of the foundation account
    | UpdateMintDistribution
      -- ^Update the distribution of newly minted GTU
    | UpdateTransactionFeeDistribution
      -- ^Update the distribution of transaction fees
    | UpdateGASRewards
    | UpdateBakerStakeThreshold
      -- ^Update the minimum stake that a baker needs to have to be able to bake
    | UpdateAddAnonymityRevoker
      -- ^Add a new anonymity revoker
    | UpdateAddIdentityProvider



-- ^Add a new identity provider


type TransactionResult
    = TransactionAccepted (List TransactionEvent)
    | TransactionRejected RejectReason


{-| Reason for transaction rejected

Must be in sync with `RejectReason` found in
<https://github.com/Concordium/concordium-base/blob/main/haskell-src/Concordium/Types/Execution.hs>

-}
type RejectReason
    = ModuleNotWF -- ^Error raised when validating the Wasm module.
    | ModuleHashAlreadyExists T.ModuleRef -- ^As the name says.
    | InvalidAccountReference T.AccountAddress -- ^Account does not exist.
    | InvalidInitMethod T.ModuleRef T.InitName -- ^Reference to a non-existing contract init method.
    | InvalidReceiveMethod T.ModuleRef T.ReceiveName -- ^Reference to a non-existing contract receive method.
    | InvalidModuleReference T.ModuleRef -- ^Reference to a non-existing module.
    | InvalidContractAddress T.ContractAddress -- ^Contract instance does not exist.
    | RuntimeFailure -- ^Runtime exception occurred when running either the init or receive method.
    | ReceiverAccountNoCredential T.AccountAddress
      -- ^The receiver account does not have a valid credential.
    | ReceiverContractNoCredential T.ContractAddress
      -- ^The receiver contract does not have a valid credential.
    | AmountTooLarge T.Address T.Amount
      -- ^When one wishes to transfer an amount from A to B but there
      -- are not enough funds on account/contract A to make this
      -- possible. The data are the from address and the amount to transfer.
    | SerializationFailure -- ^Serialization of the body failed.
    | OutOfEnergy -- ^We ran of out energy to process this transaction.
    | RejectedInit RejectReasonRejectedInit -- ^Rejected due to contract logic in init function of a contract.
    | RejectedReceive RejectReasonRejectedReceive
    | NonExistentRewardAccount T.AccountAddress -- ^Reward account desired by the baker does not exist.
    | InvalidProof -- ^Proof that the baker owns relevant private keys is not valid.
    | AlreadyABaker T.BakerId -- ^Tried to add baker for an account that already has a baker
    | NotABaker T.AccountAddress -- ^Tried to remove a baker for an account that has no baker
    | InsufficientBalanceForBakerStake -- ^The amount on the account was insufficient to cover the proposed stake
    | StakeUnderMinimumThresholdForBaking -- ^The amount provided is under the threshold required for becoming a baker
    | BakerInCooldown -- ^The change could not be made because the baker is in cooldown for another change
    | DuplicateAggregationKey T.BakerAggregationVerifyKey -- ^A baker with the given aggregation key already exists
      -- |Encountered credential ID that does not exist
    | NonExistentCredentialID
      -- |Attempted to add an account key to a key index already in use
    | KeyIndexAlreadyInUse
      -- |When the account threshold is updated, it must not exceed the amount of existing keys
    | InvalidAccountThreshold
      -- |When the credential key threshold is updated, it must not exceed the amount of existing keys
    | InvalidCredentialKeySignThreshold
      -- |Proof for an encrypted amount transfer did not validate.
    | InvalidEncryptedAmountTransferProof
      -- |Proof for a secret to public transfer did not validate.
    | InvalidTransferToPublicProof
      -- |Account tried to transfer an encrypted amount to itself, that's not allowed.
    | EncryptedAmountSelfTransfer T.AccountAddress
      -- | The provided index is below the start index or above `startIndex + length incomingAmounts`
    | InvalidIndexOnEncryptedTransfer
      -- | The transfer with schedule is going to send 0 tokens
    | ZeroScheduledAmount
      -- | The transfer with schedule has a non strictly increasing schedule
    | NonIncreasingSchedule
      -- | The first scheduled release in a transfer with schedule has already expired
    | FirstScheduledReleaseExpired
      -- | Account tried to transfer with schedule to itself, that's not allowed.
    | ScheduledSelfTransfer T.AccountAddress
      -- | At least one of the credentials was either malformed or its proof was incorrect.
    | InvalidCredentials
      -- | Some of the credential IDs already exist or are duplicated in the transaction.
    | DuplicateCredIDs (List String)
      -- | A credential id that was to be removed is not part of the account.
    | NonExistentCredIDs (List String)
      -- | Attemp to remove the first credential
    | RemoveFirstCredential
      -- | The credential holder of the keys to be updated did not sign the transaction
    | CredentialHolderDidNotSign
      -- |Account is not allowed to have multiple credentials because it contains a non-zero encrypted transfer.
    | NotAllowedMultipleCredentials
      -- |The account is not allowed to receive encrypted transfers because it has multiple credentials.
    | NotAllowedToReceiveEncrypted
      -- |The account is not allowed to send encrypted transfers (or transfer from/to public to/from encrypted)
    | NotAllowedToHandleEncrypted


type alias RejectReasonRejectedInit =
    { rejectReason : Int
    }


type alias RejectReasonRejectedReceive =
    { rejectReason : Int
    , contractAddress : T.ContractAddress
    , receiveName : T.ReceiveName
    , parameter : String
    }


decodeTransactionResult : D.Decoder TransactionResult
decodeTransactionResult =
    D.oneOf
        [ D.field "events" (D.list transactionEventsDecoder)
            |> D.map TransactionAccepted
        , D.field "rejectReason"
            (D.map TransactionRejected rejectReasonDecoder)
        ]


transactionSummaryTypeDecoder : D.Decoder TransactionSummaryType
transactionSummaryTypeDecoder =
    let
        decode tipe =
            case tipe of
                "accountTransaction" ->
                    D.succeed AccountTransaction
                        |> optional "contents" accountTransactionTypeDecoder Malformed

                "credentialDeploymentTransaction" ->
                    D.succeed CredentialDeploymentTransaction
                        |> required "contents" credentialTypeDecoder

                "updateTransaction" ->
                    D.succeed UpdateTransaction
                        |> required "contents" updateTypeDecoder

                _ ->
                    D.fail <| "Unknown TransactionSummaryType type: " ++ tipe
    in
    D.field "type" D.string |> D.andThen decode


accountTransactionTypeDecoder : D.Decoder AccountTransactionType
accountTransactionTypeDecoder =
    let
        decode tipe =
            case tipe of
                "deployModule" ->
                    D.succeed DeployModule

                "initContract" ->
                    D.succeed InitContract

                "update" ->
                    D.succeed Update

                "transfer" ->
                    D.succeed Transfer

                "addBaker" ->
                    D.succeed AddBaker

                "removeBaker" ->
                    D.succeed RemoveBaker

                "updateBakerStake" ->
                    D.succeed UpdateBakerStake

                "updateBakerRestakeEarnings" ->
                    D.succeed UpdateBakerRestakeEarnings

                "updateBakerKeys" ->
                    D.succeed UpdateBakerKeys

                "updateCredentialKeys" ->
                    D.succeed UpdateCredentialKeys

                "encryptedAmountTransfer" ->
                    D.succeed EncryptedAmountTransfer

                "transferToEncrypted" ->
                    D.succeed TransferToEncrypted

                "transferToPublic" ->
                    D.succeed TransferToPublic

                "transferWithSchedule" ->
                    D.succeed TransferWithSchedule

                "updateCredentials" ->
                    D.succeed UpdateCredentials

                "registerData" ->
                    D.succeed RegisterData

                _ ->
                    D.fail <| "Unknown AccountTransaction type: " ++ tipe
    in
    D.string |> D.andThen decode


updateTypeDecoder : D.Decoder UpdateType
updateTypeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "updateRootKeysWithRootKeys" ->
                        D.succeed UpdateRootKeysWithRootKeys

                    "updateLevel1KeysWithRootKeys" ->
                        D.succeed UpdateLevel1KeysWithRootKeys

                    "updateLevel2KeysWithRootKeys" ->
                        D.succeed UpdateLevel2KeysWithRootKeys

                    "updateLevel1KeysWithLevel1Keys" ->
                        D.succeed UpdateLevel1KeysWithLevel1Keys

                    "updateLevel2KeysWithLevel1Keys" ->
                        D.succeed UpdateLevel2KeysWithLevel1Keys

                    "updateProtocol" ->
                        D.succeed UpdateProtocol

                    "updateElectionDifficulty" ->
                        D.succeed UpdateElectionDifficulty

                    "updateEuroPerEnergy" ->
                        D.succeed UpdateEuroPerEnergy

                    "updateMicroGTUPerEuro" ->
                        D.succeed UpdateMicroGTUPerEuro

                    "updateFoundationAccount" ->
                        D.succeed UpdateFoundationAccount

                    "updateMintDistribution" ->
                        D.succeed UpdateMintDistribution

                    "updateTransactionFeeDistribution" ->
                        D.succeed UpdateTransactionFeeDistribution

                    "updateGASRewards" ->
                        D.succeed UpdateGASRewards

                    "updateBakerStakeThreshold" ->
                        D.succeed UpdateBakerStakeThreshold

                    "updateAddAnonymityRevoker" ->
                        D.succeed UpdateAddAnonymityRevoker

                    "updateAddIdentityProvider" ->
                        D.succeed UpdateAddIdentityProvider

                    _ ->
                        D.fail <| "Unknown UpdateType type: " ++ str
            )


credentialTypeDecoder : D.Decoder CredentialType
credentialTypeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "initial" ->
                        D.succeed Initial

                    "normal" ->
                        D.succeed Normal

                    _ ->
                        D.fail "Invalid credential type"
            )


decodeNumerousToString : D.Decoder String
decodeNumerousToString =
    D.oneOf
        [ D.string
        , D.int |> D.map String.fromInt
        , T.addressDecoder |> D.map T.addressToString
        ]


transactionSummaryDecoder : D.Decoder TransactionSummary
transactionSummaryDecoder =
    D.succeed TransactionSummary
        |> required "hash" D.string
        |> required "sender" (D.nullable T.accountAddressDecoder)
        |> required "cost" T.decodeAmount
        |> required "result" decodeTransactionResult
        |> required "energyCost" T.decodeEnergy
        |> required "type" transactionSummaryTypeDecoder


rejectReasonDecoder : D.Decoder RejectReason
rejectReasonDecoder =
    let
        decode tag =
            case tag of
                "ModuleNotWF" ->
                    D.succeed ModuleNotWF

                "ModuleHashAlreadyExists" ->
                    D.map ModuleHashAlreadyExists <|
                        D.field "contents" D.string

                "InvalidAccountReference" ->
                    D.map InvalidAccountReference <|
                        D.field "contents" T.accountAddressDecoder

                "InvalidInitMethod" ->
                    D.succeed InvalidInitMethod
                        |> custom (D.index 0 D.string)
                        |> custom (D.index 1 T.contractInitNameDecoder)
                        |> D.field "contents"

                "InvalidReceiveMethod" ->
                    D.succeed InvalidReceiveMethod
                        |> custom (D.index 0 D.string)
                        |> custom (D.index 1 T.contractReceiveNameDecoder)
                        |> D.field "contents"

                "InvalidModuleReference" ->
                    D.map InvalidModuleReference <|
                        D.field "contents" D.string

                "InvalidContractAddress" ->
                    D.map InvalidContractAddress <|
                        D.field "contents" T.contractAddressDecoder

                "RuntimeFailure" ->
                    D.succeed RuntimeFailure

                "ReceiverAccountNoCredential" ->
                    D.map ReceiverAccountNoCredential <|
                        D.field "contents" T.accountAddressDecoder

                "ReceiverContractNoCredential" ->
                    D.map ReceiverContractNoCredential <|
                        D.field "contents" T.contractAddressDecoder

                "AmountTooLarge" ->
                    D.succeed AmountTooLarge
                        |> custom (D.index 0 T.addressDecoder)
                        |> custom (D.index 1 T.decodeAmount)
                        |> D.field "contents"

                "SerializationFailure" ->
                    D.succeed SerializationFailure

                "OutOfEnergy" ->
                    D.succeed OutOfEnergy

                "RejectedInit" ->
                    D.succeed RejectReasonRejectedInit
                        |> required "rejectReason" D.int
                        |> D.map RejectedInit

                "RejectedReceive" ->
                    D.succeed RejectReasonRejectedReceive
                        |> required "rejectReason" D.int
                        |> required "contractAddress" T.contractAddressDecoder
                        |> required "receiveName" T.contractReceiveNameDecoder
                        |> required "parameter" D.string
                        |> D.map RejectedReceive

                "NonExistentRewardAccount" ->
                    D.map NonExistentRewardAccount <|
                        D.field "contents" T.accountAddressDecoder

                "InvalidProof" ->
                    D.succeed InvalidProof

                "AlreadyABaker" ->
                    D.map AlreadyABaker <|
                        D.field "contents" D.int

                "NotABaker" ->
                    D.map NotABaker <|
                        D.field "contents" T.accountAddressDecoder

                "InsufficientBalanceForBakerStake" ->
                    D.succeed InsufficientBalanceForBakerStake

                "StakeUnderMinimumThresholdForBaking" ->
                    D.succeed StakeUnderMinimumThresholdForBaking

                "BakerInCooldown" ->
                    D.succeed BakerInCooldown

                "DuplicateAggregationKey" ->
                    D.map DuplicateAggregationKey <|
                        D.field "contents" D.string

                "NonExistentCredentialID" ->
                    D.succeed NonExistentCredentialID

                "KeyIndexAlreadyInUse" ->
                    D.succeed KeyIndexAlreadyInUse

                "InvalidAccountThreshold" ->
                    D.succeed InvalidAccountThreshold

                "InvalidCredentialKeySignThreshold" ->
                    D.succeed InvalidCredentialKeySignThreshold

                "InvalidEncryptedAmountTransferProof" ->
                    D.succeed InvalidEncryptedAmountTransferProof

                "InvalidTransferToPublicProof" ->
                    D.succeed InvalidTransferToPublicProof

                "EncryptedAmountSelfTransfer" ->
                    D.map EncryptedAmountSelfTransfer <|
                        D.field "contents" T.accountAddressDecoder

                "InvalidIndexOnEncryptedTransfer" ->
                    D.succeed InvalidIndexOnEncryptedTransfer

                "ZeroScheduledAmount" ->
                    D.succeed ZeroScheduledAmount

                "NonIncreasingSchedule" ->
                    D.succeed NonIncreasingSchedule

                "FirstScheduledReleaseExpired" ->
                    D.succeed FirstScheduledReleaseExpired

                "ScheduledSelfTransfer" ->
                    D.map ScheduledSelfTransfer <|
                        D.field "contents" T.accountAddressDecoder

                "DuplicateCredIDs" ->
                    D.map DuplicateCredIDs <|
                        D.field "contents" (D.list D.string)

                "NonExistentCredIDs" ->
                    D.map NonExistentCredIDs <|
                        D.field "contents" (D.list D.string)

                "InvalidCredentials" ->
                    D.succeed InvalidCredentials

                "RemoveFirstCredential" ->
                    D.succeed RemoveFirstCredential

                "CredentialHolderDidNotSign" ->
                    D.succeed CredentialHolderDidNotSign

                "NotAllowedMultipleCredentials" ->
                    D.succeed NotAllowedMultipleCredentials

                "NotAllowedToReceiveEncrypted" ->
                    D.succeed NotAllowedToReceiveEncrypted

                "NotAllowedToHandleEncrypted" ->
                    D.succeed NotAllowedToHandleEncrypted

                _ ->
                    D.fail <| "Unknown RejectReason: " ++ tag
    in
    D.field "tag" D.string |> D.andThen decode
