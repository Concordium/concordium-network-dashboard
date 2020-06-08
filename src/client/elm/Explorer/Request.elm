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
    { bestBlock : String
    , genesisTime : Posix
    , epochDuration : Int
    }


consensusStatusDecoder =
    D.succeed ConsensusStatus
        |> required "bestBlock" D.string
        |> required "genesisTime" Iso8601.decoder
        |> required "epochDuration" D.int


getConsensusStatus : (Result Http.Error ConsensusStatus -> msg) -> Cmd msg
getConsensusStatus msg =
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



-- BlockInfo


getBlockInfo blockhash msg =
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
            { specialEvents = []
            , transactionSummaries = []
            , finalizationData = Nothing
            }


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


blockInfoStub : BlockInfo
blockInfoStub =
    { transactionsSize = 0
    , blockParent = "957c2ae05d82e9ba30142e02cda3b3c2ab779329d359c665abc7059dbb88cc61"
    , blockHash = "d06708ea234df3189aa212008d8f0a97ba68384482d25fbeebdc2c822421f8ff"
    , finalized = False
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


getBlockInfoResponseStub =
    """
{
  "transactionsSize": 0,
  "blockParent": "ec391232301e51b067619284952ab0d1bfd2c1cb077643e53114e8709dd2bd2e",
  "blockHash": "f911f25086e40ccf32a22b9e5926675fef49f4b5e450d2625d1159c02ff2f48b",
  "finalized": true,
  "blockArriveTime": "2020-05-13T13:55:25.049178983Z",
  "blockReceiveTime": "2020-05-13T13:55:24.906362438Z",
  "transactionCount": 0,
  "transactionEnergyCost": 0,
  "blockSlot": 15877895342,
  "blockLastFinalized": "5b393e1e20eb446a2069bcd3bbe9c8b22f2e731e742b1b618f3184377fd1eaa2",
  "blockSlotTime": "2070-08-27T15:49:30.2Z",
  "blockHeight": 43756,
  "blockBaker": 3
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
    , finalizationData : Maybe FinalizationData
    }


blockSummaryDecoder : D.Decoder BlockSummary
blockSummaryDecoder =
    D.succeed BlockSummary
        |> required "specialEvents" (D.list specialEventDecoder)
        |> required "transactionSummaries" (D.list transactionSummaryDecoder)
        |> required "finalizationData" (D.nullable finalizationDataDecoder)


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


type alias FinalizationData =
    { blockPointer : String
    , index : Int
    , delay : Int
    , finalizers : List FinalizerInfo
    }


finalizationDataDecoder : D.Decoder FinalizationData
finalizationDataDecoder =
    D.succeed FinalizationData
        |> required "finalizationBlockPointer" D.string
        |> required "finalizationIndex" D.int
        |> required "finalizationDelay" D.int
        |> required "finalizers" (D.list finalizerInfoDecoder)


type alias FinalizerInfo =
    { bakerId : Int
    , weight : Int
    , signed : Bool
    }


finalizerInfoDecoder : D.Decoder FinalizerInfo
finalizerInfoDecoder =
    D.succeed FinalizerInfo
        |> required "bakerId" D.int
        |> required "weight" D.int
        |> required "signed" D.bool


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
