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
<https://gitlab.com/Concordium/consensus/globalstate-types/-/blob/master/src/Concordium/Types/Execution.hs#L596>

-}
type TransactionSummaryType
    = AccountTransaction AccountTransactionType -- Contains a constructor `Malformed` to represent `Nothing` in the corresponding type in haskell
    | CredentialDeploymentTransaction CredentialType
    | UpdateTransaction UpdateType


{-| Reason for transaction rejected

Must be in sync with the constructor names of `Payload` found in
<https://gitlab.com/Concordium/consensus/globalstate-types/-/blob/master/src/Concordium/Types/Execution.hs#L198>

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
    | UpdateAccountKeys
    | AddAccountKeys
    | RemoveAccountKeys
    | EncryptedAmountTransfer
    | TransferToEncrypted
    | TransferToPublic
    | TransferWithSchedule
    | Malformed


type CredentialType
    = Initial
    | Normal


{-| Must be in sync with `UpdateType` found in
<https://gitlab.com/Concordium/consensus/globalstate-types/-/blob/master/src/Concordium/Types/Updates.hs#L375>
-}
type UpdateType
    = UpdateAuthorization
      -- ^Update the access structures that authorize updates
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


type TransactionResult
    = TransactionAccepted (List TransactionEvent)
    | TransactionRejected RejectReason


{-| Reason for transaction rejected

Must be in sync with `RejectReason` found in
<https://gitlab.com/Concordium/consensus/globalstate-types/-/blob/master/src/Concordium/Types/Execution.hs#L499>

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
    | Rejected -- ^Rejected due to contract logic.
    | NonExistentRewardAccount T.AccountAddress -- ^Reward account desired by the baker does not exist.
    | InvalidProof -- ^Proof that the baker owns relevant private keys is not valid.
    | AlreadyABaker T.BakerId -- ^Tried to add baker for an account that already has a baker
    | NotABaker T.AccountAddress -- ^Tried to remove a baker for an account that has no baker
    | InsufficientBalanceForBakerStake -- ^The amount on the account was insufficient to cover the proposed stake
    | BakerInCooldown -- ^The change could not be made because the baker is in cooldown for another change
    | DuplicateAggregationKey T.BakerAggregationVerifyKey -- ^A baker with the given aggregation key already exists
      -- |Encountered index to which no account key belongs when removing or updating keys
    | NonExistentAccountKey
      -- |Attempted to add an account key to a key index already in use
    | KeyIndexAlreadyInUse
      -- |When the account key threshold is updated, it must not exceed the amount of existing keys
    | InvalidAccountKeySignThreshold
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

                "updateAccountKeys" ->
                    D.succeed UpdateAccountKeys

                "addAccountKeys" ->
                    D.succeed AddAccountKeys

                "removeAccountKeys" ->
                    D.succeed RemoveAccountKeys

                "encryptedAmountTransfer" ->
                    D.succeed EncryptedAmountTransfer

                "transferToEncrypted" ->
                    D.succeed TransferToEncrypted

                "transferToPublic" ->
                    D.succeed TransferToPublic

                "transferWithSchedule" ->
                    D.succeed TransferWithSchedule

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
                    "updateAuthorization" ->
                        D.succeed UpdateAuthorization

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

                "Rejected" ->
                    D.succeed Rejected

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

                "BakerInCooldown" ->
                    D.succeed BakerInCooldown

                "DuplicateAggregationKey" ->
                    D.map DuplicateAggregationKey <|
                        D.field "contents" D.string

                "NonExistentAccountKey" ->
                    D.succeed NonExistentAccountKey

                "KeyIndexAlreadyInUse" ->
                    D.succeed KeyIndexAlreadyInUse

                "InvalidAccountKeySignThreshold" ->
                    D.succeed InvalidAccountKeySignThreshold

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

                _ ->
                    D.fail <| "Unknown RejectReason: " ++ tag
    in
    D.field "tag" D.string |> D.andThen decode
