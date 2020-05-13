module Explorer.Request exposing (..)

import Config
import Explorer.Stubs exposing (..)
import Http exposing (..)
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (..)
import Task
import Time exposing (Posix)
import Transaction.Event exposing (..)
import Transaction.Summary exposing (..)



-- ConsensusStatus


type alias ConsensusStatus =
    { bestBlock : String }


consensusStatusDecoder =
    D.succeed ConsensusStatus
        |> required "bestBlock" D.string


getConsensusStatus : (Result Http.Error ConsensusStatus -> msg) -> Cmd msg
getConsensusStatus msg =
    -- let
    --     x =
    --         Debug.log "calling" "get consensus status!"
    -- in
    Http.get
        { url = Config.middleware ++ "/v1/consensusStatus"
        , expect = expectJson_ msg consensusStatusDecoder
        }



-- Block


type alias Block =
    { hash : String
    , blockInfo : WebData BlockInfo
    , blockSummary : WebData BlockSummary
    }


initBlock =
    { hash = "<empty hash>"
    , blockInfo = NotAsked
    , blockSummary = NotAsked
    }


stubBlock =
    { hash = "957c2ae05d82e9ba30142e02cda3b3c2ab779329d359c665abc7059dbb88cc61"
    , blockInfo = Success getBlockInfoStub_
    , blockSummary = Success getBlockSummaryStub_
    }



-- BlockInfo


getBlockInfo blockhash msg =
    -- let
    --     x =
    --         Debug.log "calling" "get Block info!"
    -- in
    Http.get
        { url = Config.middleware ++ "/v1/blockInfo/" ++ blockhash
        , expect = expectJson_ msg blockInfoDecoder
        }


getBlockInfoStub blockhash msg =
    case D.decodeString blockInfoDecoder getBlockInfoResponseStub of
        Ok blockInfo ->
            trigger (msg blockInfo)

        Err err ->
            -- let
            --     x =
            --         Debug.log "getBlockInfoStub decoding" (D.errorToString err)
            -- in
            Cmd.none


getBlockInfoStub_ blockhash =
    case D.decodeString blockInfoDecoder getBlockInfoResponseStub of
        Ok blockInfo ->
            blockInfo

        Err err ->
            -- let
            --     x =
            --         Debug.log "getBlockInfoStub decoding" (D.errorToString err)
            -- in
            blockInfoStub



-- BlockSummary


getBlockSummary blockhash msg =
    -- let
    --     x =
    --         Debug.log "calling" "get Block summary!"
    -- in
    Http.get
        { url = Config.middleware ++ "/v1/blockSummary/" ++ blockhash
        , expect = expectJson_ msg blockSummaryDecoder
        }


getBlockSummaryStub_ =
    case D.decodeString blockSummaryDecoder getBlockSummary_stub_allTransactions of
        Ok blockSummary ->
            blockSummary

        Err err ->
            -- let
            --     x =
            --         Debug.log "getBlockSummaryStub decoding" (D.errorToString err)
            -- in
            { specialEvents = [], transactionSummaries = [] }


type alias BlockInfo =
    { transactionsSize : Int
    , blockParent : String
    , mintedAmountPerSlot : Int
    , totalEncryptedAmount : Int
    , blockHash : String
    , finalized : Bool
    , totalAmount : Int
    , blockArriveTime : Posix
    , blockReceiveTime : Posix
    , transactionCount : Int
    , transactionEnergyCost : Int
    , blockSlot : Int
    , blockLastFinalized : String
    , blockSlotTime : Posix
    , blockHeight : Int
    , blockBaker : Int
    , executionCost : Int
    , centralBankAmount : Int
    }


blockInfoStub : BlockInfo
blockInfoStub =
    { transactionsSize = 0
    , blockParent = "957c2ae05d82e9ba30142e02cda3b3c2ab779329d359c665abc7059dbb88cc61"
    , mintedAmountPerSlot = 100
    , totalEncryptedAmount = 0
    , blockHash = "d06708ea234df3189aa212008d8f0a97ba68384482d25fbeebdc2c822421f8ff"
    , finalized = False
    , totalAmount = 15000628024800
    , blockArriveTime =
        Iso8601.toTime "2020-04-05T17:04:10.8763399Z"
            |> Result.withDefault (Time.millisToPosix 1586459410)
    , blockReceiveTime =
        Iso8601.toTime "2020-04-05T17:04:10.8763399Z"
            |> Result.withDefault (Time.millisToPosix 1586459410)
    , transactionCount = 1
    , transactionEnergyCost = 0
    , blockSlot = 6280248
    , blockLastFinalized = "a0cdd5b7e51d83bef2d0ed55cb35cdc8c42280add22e9e59c893ecb492ff609a"
    , blockSlotTime =
        Iso8601.toTime "2020-04-05T16:08:00Z"
            |> Result.withDefault (Time.millisToPosix 1586459410)
    , blockHeight = 79
    , blockBaker = 2
    , executionCost = 0
    , centralBankAmount = 474
    }


blockInfoDecoder : D.Decoder BlockInfo
blockInfoDecoder =
    D.succeed BlockInfo
        |> required "transactionsSize" D.int
        |> required "blockParent" D.string
        |> required "mintedAmountPerSlot" D.int
        |> required "totalEncryptedAmount" D.int
        |> required "blockHash" D.string
        |> required "finalized" D.bool
        |> required "totalAmount" D.int
        |> required "blockArriveTime" Iso8601.decoder
        |> required "blockReceiveTime" Iso8601.decoder
        |> required "transactionCount" D.int
        |> required "transactionEnergyCost" D.int
        |> required "blockSlot" D.int
        |> required "blockLastFinalized" D.string
        |> required "blockSlotTime" Iso8601.decoder
        |> required "blockHeight" D.int
        |> required "blockBaker" D.int
        |> required "executionCost" D.int
        |> required "centralBankAmount" D.int


getBlockInfoResponseStub =
    """
{
  "transactionsSize": 0,
  "blockParent": "d06708ea234df3189aa212008d8f0a97ba68384482d25fbeebdc2c822421f8ff",
  "mintedAmountPerSlot": 100,
  "totalEncryptedAmount": 0,
  "blockHash": "957c2ae05d82e9ba30142e02cda3b3c2ab779329d359c665abc7059dbb88cc61",
  "finalized": true,
  "totalAmount": 15000628024800,
  "blockArriveTime": "2020-03-05T17:04:10.8763399Z",
  "blockReceiveTime": "2020-03-05T17:04:10.8763399Z",
  "transactionCount": 0,
  "transactionEnergyCost": 0,
  "blockSlot": 6280248,
  "blockLastFinalized": "a0cdd5b7e51d83bef2d0ed55cb35cdc8c42280add22e9e59c893ecb492ff609a",
  "blockSlotTime": "2020-03-05T16:08:00Z",
  "blockHeight": 79,
  "blockBaker": 2,
  "executionCost": 0,
  "centralBankAmount": 474
}
"""


getBlockSumaryResponseStub =
    """
{
  "specialEvents": [
    {
      "bakerId": 2,
      "rewardAmount": 474,
      "bakerAccount": "2yFPDKmDDYxSep2UWHQyzNuNAZ1RU4EXGih7itesLziKcuQRAG"
    }
  ],
  "transactionSummaries": []
}
"""


trigger : msg -> Cmd msg
trigger msg =
    Task.perform identity (Task.succeed msg)


type alias BlockSummary =
    { specialEvents : List SpecialEvent
    , transactionSummaries : List TransactionSummary
    }


blockSummaryDecoder : D.Decoder BlockSummary
blockSummaryDecoder =
    D.succeed BlockSummary
        |> required "specialEvents" (D.list specialEventDecoder)
        |> required "transactionSummaries" (D.list transactionSummaryDecoder)


type alias SpecialEvent =
    { bakerId : Int
    , rewardAmount : Int
    , bakerAccount : String
    }


specialEventDecoder : D.Decoder SpecialEvent
specialEventDecoder =
    D.succeed SpecialEvent
        |> required "bakerId" D.int
        |> required "rewardAmount" D.int
        |> required "bakerAccount" D.string


{-| The default Http.expectJson / Http.expectString don't allow you to see any body
returned in an error (i.e. 403) states. The following docs;
<https://package.elm-lang.org/packages/elm/http/latest/Http#expectStringResponse>
describe our sitution perfectly, so that's that code below, with a modified
Http.BadStatus\_ handler to map it to BadBody String instead of BadStatus Int
so we can actually see the error message if the JSON doesn't parse
-}
expectJson_ : (Result Http.Error a -> msg) -> D.Decoder a -> Expect msg
expectJson_ toMsg decoder =
    expectStringResponse toMsg <|
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


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl url ->
            "HTTP Malformed url: " ++ url

        Timeout ->
            "HTTP Timeout exceeded"

        NetworkError ->
            "HTTP Network error"

        BadStatus code ->
            "Unexpected HTTP response code: " ++ String.fromInt code

        BadBody text ->
            "Unexpected HTTP response: " ++ text
