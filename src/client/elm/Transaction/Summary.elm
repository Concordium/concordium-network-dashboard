module Transaction.Summary exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
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
                -- https://gitlab.com/Concordium/consensus/globalstate-types/-/blob/master/src/Concordium/Types/Execution.hs#L499
                -- There are a number of possible RejectReasons. Rather than mapping them all, for now
                -- we loosely convert them all to strings for quick display
                |> optional "contents"
                    (D.oneOf
                        [ decodeNumerousToString
                        , D.list decodeNumerousToString |> D.map (String.join ", ")
                        ]
                    )
                    ""
            )
        ]


decodeNumerousToString : D.Decoder String
decodeNumerousToString =
    D.oneOf
        [ D.string
        , D.int |> D.map String.fromInt
        , accountInfoDecoder |> D.map accountInfoAddress
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
