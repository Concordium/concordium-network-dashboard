module Transaction.Summary exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Transaction.Event exposing (..)


type alias TransactionSummary =
    { hash : String
    , sender : Maybe String
    , cost : Int
    , result : TransactionResult
    , energyCost : Int
    , tipe : Maybe String
    }


type TransactionResult
    = TransactionAccepted (List TransactionEvent)
    | TransactionRejected String String -- String:Tag, String:Contents


decodeTransactionResult : D.Decoder TransactionResult
decodeTransactionResult =
    D.oneOf
        [ D.field "events" (D.list transactionEventsDecoder)
            |> D.map TransactionAccepted
        , D.field "rejectReason"
            (D.succeed TransactionRejected
                |> required "tag" D.string
                -- This is unfortunate but seems depending on the type of transaction,
                -- the value of "contents" can be either a string or an int
                |> optional "contents" (D.oneOf [ D.string, D.int |> D.map String.fromInt ]) ""
            )
        ]


transactionSummaryDecoder : D.Decoder TransactionSummary
transactionSummaryDecoder =
    D.succeed TransactionSummary
        |> required "hash" D.string
        |> required "sender" (D.nullable D.string)
        |> required "cost" D.int
        |> required "result" decodeTransactionResult
        |> required "energyCost" D.int
        |> required "type" (D.nullable D.string)
