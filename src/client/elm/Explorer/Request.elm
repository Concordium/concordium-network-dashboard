module Explorer.Request exposing (..)

import Config
import Http exposing (..)
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Task
import Time exposing (Posix)
import Transaction.Summary exposing (..)



-- ConsensusStatus


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


getConsensusStatus : (Result Http.Error ConsensusStatus -> msg) -> Cmd msg
getConsensusStatus msg =
    Http.get
        { url = Config.middleware ++ "/v1/consensusStatus"
        , expect = expectJson_ msg consensusStatusDecoder
        }



-- BlockInfo


getBlockInfo : String -> (Result Error BlockInfo -> msg) -> Cmd msg
getBlockInfo blockhash msg =
    Http.get
        { url = Config.middleware ++ "/v1/blockInfo/" ++ blockhash
        , expect = expectJson_ msg blockInfoDecoder
        }



-- BlockSummary


getBlockSummary : String -> (Result Error BlockSummary -> msg) -> Cmd msg
getBlockSummary blockhash msg =
    -- let
    --     x =
    --         Debug.log "calling" "get Block summary!"
    -- in
    Http.get
        { url = Config.middleware ++ "/v1/blockSummary/" ++ blockhash
        , expect = expectJson_ msg blockSummaryDecoder
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
        |> optional "finalizationData" (D.nullable finalizationDataDecoder) Nothing


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
