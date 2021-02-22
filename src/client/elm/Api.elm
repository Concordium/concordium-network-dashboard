module Api exposing (..)

{-| Functions and types for making API requests to the middleware.
-}

import Dict exposing (Dict)
import Http
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Time exposing (Posix)
import Transaction.Summary as TxSummary
import Types as T


type alias Config =
    { middlewareUrl : String
    }


type alias ApiResult a =
    Result Http.Error a


{-| Make a GET request to the middleware
-}
getMiddleware : Config -> String -> D.Decoder a -> (ApiResult a -> msg) -> Cmd msg
getMiddleware cfg url decoder msg =
    Http.get { url = cfg.middlewareUrl ++ url, expect = expectJson_ msg decoder }


{-| The default Http.expectJson / Http.expectString don't allow you to see any body
returned in an error (i.e. 403) states. The following docs;
<https://package.elm-lang.org/packages/elm/http/latest/Http#expectStringResponse>
describe our sitution perfectly, so that's that code below, with a modified
Http.BadStatus\_ handler to map it to BadBody String instead of BadStatus Int
so we can actually see the error message if the JSON doesn't parse
-}
expectJson_ : (ApiResult a -> msg) -> D.Decoder a -> Http.Expect msg
expectJson_ toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (String.fromInt metadata.statusCode ++ ": " ++ body))

                Http.GoodStatus_ metadata body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (D.errorToString err))



-- ConsensusStatus


getConsensusStatus : Config -> (ApiResult ConsensusStatus -> msg) -> Cmd msg
getConsensusStatus cfg msg =
    getMiddleware cfg "/v1/consensusStatus" consensusStatusDecoder msg


type alias ConsensusStatus =
    { bestBlock : String
    , genesisTime : Posix
    , epochDuration : Int
    }


consensusStatusDecoder : D.Decoder ConsensusStatus
consensusStatusDecoder =
    D.succeed ConsensusStatus
        |> required "bestBlock" D.string
        |> required "genesisTime" Iso8601.decoder
        |> required "epochDuration" D.int



-- BlockInfo


getBlockInfo : Config -> T.BlockHash -> (ApiResult BlockInfo -> msg) -> Cmd msg
getBlockInfo cfg blockhash msg =
    getMiddleware cfg ("/v1/blockInfo/" ++ blockhash) blockInfoDecoder msg


type alias BlockInfo =
    { transactionsSize : Int
    , blockParent : String
    , blockHash : String
    , finalized : Bool
    , blockArriveTime : Posix
    , blockReceiveTime : Posix
    , transactionCount : Int
    , transactionEnergyCost : Int
    , blockSlot : Int
    , blockLastFinalized : String
    , blockSlotTime : Posix
    , blockHeight : Int
    , blockBaker : Int
    }


blockInfoDecoder : D.Decoder BlockInfo
blockInfoDecoder =
    D.succeed BlockInfo
        |> required "transactionsSize" D.int
        |> required "blockParent" D.string
        |> required "blockHash" D.string
        |> required "finalized" D.bool
        |> required "blockArriveTime" Iso8601.decoder
        |> required "blockReceiveTime" Iso8601.decoder
        |> required "transactionCount" D.int
        |> required "transactionEnergyCost" D.int
        |> required "blockSlot" D.int
        |> required "blockLastFinalized" D.string
        |> required "blockSlotTime" Iso8601.decoder
        |> required "blockHeight" D.int
        |> required "blockBaker" D.int



-- Get transaction status


getTransactionStatus : Config -> T.TxHash -> (ApiResult TransactionStatusResponse -> msg) -> Cmd msg
getTransactionStatus cfg txHash msg =
    getMiddleware cfg ("/v1/transactionStatus/" ++ txHash) transactionStatusResponseDecoder msg


{-| The current status of a transaction where:

`Received` means transaction is received by the node, but not in a block yet,
`Committed` means Transaction is committed to one or more blocks and
`Finalized` means Transaction is finalized in a block

-}
type TransactionStatus
    = Received
    | Committed (Dict T.BlockHash TxSummary.TransactionSummary)
    | Finalized ( T.BlockHash, TxSummary.TransactionSummary )


{-| The response from getting the transaction status.

Since the middleware will either return the transaction status or a string
with the error as the body.

-}
type TransactionStatusResponse
    = Status TransactionStatus
    | InvalidTransactionHash


transactionStatusDecoder : D.Decoder TransactionStatus
transactionStatusDecoder =
    let
        decode str =
            case str of
                "received" ->
                    D.succeed Received

                "committed" ->
                    D.dict TxSummary.transactionSummaryDecoder
                        |> D.field "outcomes"
                        |> D.map Committed

                "finalized" ->
                    D.dict TxSummary.transactionSummaryDecoder
                        |> D.field "outcomes"
                        |> D.andThen
                            (Dict.toList
                                >> List.head
                                >> Maybe.map (D.succeed << Finalized)
                                >> Maybe.withDefault (D.fail "Invalid transaction status finalized")
                            )

                status ->
                    D.fail <| "Unknown transaction status:" ++ status
    in
    D.field "status" D.string |> D.andThen decode


transactionStatusResponseDecoder : D.Decoder TransactionStatusResponse
transactionStatusResponseDecoder =
    D.oneOf
        [ D.map Status transactionStatusDecoder
        , D.string
            |> D.andThen
                (\str ->
                    if str == "Invalid transaction hash." then
                        D.succeed InvalidTransactionHash

                    else
                        D.fail "Unknown transaction status response"
                )
        ]
