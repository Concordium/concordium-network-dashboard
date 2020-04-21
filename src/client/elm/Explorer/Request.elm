module Explorer.Request exposing (..)

import Config
import Http exposing (..)
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (..)
import Task
import Time exposing (Posix)



-- ConsensusStatus


type alias ConsensusStatus =
    { bestBlock : String }


consensusStatusDecoder =
    D.succeed ConsensusStatus
        |> required "bestBlock" D.string


getConsensusStatus : (Result Http.Error ConsensusStatus -> msg) -> Cmd msg
getConsensusStatus msg =
    let
        x =
            Debug.log "calling" "get consensus status!"
    in
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
    let
        x =
            Debug.log "calling" "get Block info!"
    in
    Http.get
        { url = Config.middleware ++ "/v1/blockInfo/" ++ blockhash
        , expect = expectJson_ msg blockInfoDecoder
        }


getBlockInfoStub blockhash msg =
    case D.decodeString blockInfoDecoder getBlockInfoResponseStub of
        Ok blockInfo ->
            trigger (msg blockInfo)

        Err err ->
            let
                x =
                    Debug.log "getBlockInfoStub decoding" (D.errorToString err)
            in
            Cmd.none


getBlockInfoStub_ blockhash =
    case D.decodeString blockInfoDecoder getBlockInfoResponseStub of
        Ok blockInfo ->
            blockInfo

        Err err ->
            let
                x =
                    Debug.log "getBlockInfoStub decoding" (D.errorToString err)
            in
            blockInfoStub



-- BlockSummary


getBlockSummary blockhash msg =
    let
        x =
            Debug.log "calling" "get Block summary!"
    in
    Http.get
        { url = Config.middleware ++ "/v1/blockSummary/" ++ blockhash
        , expect = expectJson_ msg blockSummaryDecoder
        }


getBlockSummaryStub_ =
    case D.decodeString blockSummaryDecoder getBlockSummary_stub_allTransactions of
        Ok blockSummary ->
            blockSummary

        Err err ->
            let
                x =
                    Debug.log "getBlockSummaryStub decoding" (D.errorToString err)
            in
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
      "bakerid": 2,
      "rewardamount": 474,
      "bakeraccount": "2yFPDKmDDYxSep2UWHQyzNuNAZ1RU4EXGih7itesLziKcuQRAG"
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
    { bakerid : Int
    , rewardamount : Int
    , bakeraccount : String
    }


specialEventDecoder : D.Decoder SpecialEvent
specialEventDecoder =
    D.succeed SpecialEvent
        |> required "bakerid" D.int
        |> required "rewardamount" D.int
        |> required "bakeraccount" D.string


type alias TransactionSummary =
    { hash : String
    , sender : Maybe String
    , cost : Int
    , result : TransactionResult
    , energycost : Int
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
        , D.field "rejectreason"
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
        |> required "energycost" D.int
        |> required "type" (D.nullable D.string)


type TransactionEvent
    = TransactionEventTransfer EventTransfer
    | TransactionEventAccountCreated EventAccountCreated
    | TransactionEventCredentialDeployed EventCredentialDeployed
    | TransactionEventStakeDelegated EventStakeDelegated
    | TransactionEventBakerAdded EventBakerAdded


transactionEventsDecoder : D.Decoder TransactionEvent
transactionEventsDecoder =
    D.oneOf
        [ D.map TransactionEventTransfer eventTransferDecoder
        , D.map TransactionEventAccountCreated eventAccountCreatedDecoder
        , D.map TransactionEventCredentialDeployed eventCredentialDeployedDecoder
        , D.map TransactionEventStakeDelegated eventStakeDelegatedDecoder
        , D.map TransactionEventBakerAdded eventBakerAddedDecoder
        ]


type alias EventTransfer =
    { amount : Int
    , tag : String
    , to : AccountInfo
    , from : AccountInfo
    }


eventTransferDecoder : D.Decoder EventTransfer
eventTransferDecoder =
    D.succeed EventTransfer
        |> required "amount" D.int
        |> required "tag" D.string
        |> required "to" accountInfoDecoder
        |> required "from" accountInfoDecoder


type alias EventAccountCreated =
    { tag : String
    , account : String
    }


eventAccountCreatedDecoder : D.Decoder EventAccountCreated
eventAccountCreatedDecoder =
    D.succeed EventAccountCreated
        |> required "tag" D.string
        |> required "contents" D.string


type alias EventCredentialDeployed =
    { tag : String
    , regid : String
    , account : String
    }


eventCredentialDeployedDecoder : D.Decoder EventCredentialDeployed
eventCredentialDeployedDecoder =
    D.succeed EventCredentialDeployed
        |> required "tag" D.string
        |> required "regid" D.string
        |> required "account" D.string


type alias EventStakeDelegated =
    { tag : String
    , account : String
    , baker : Int
    }


eventStakeDelegatedDecoder : D.Decoder EventStakeDelegated
eventStakeDelegatedDecoder =
    D.succeed EventStakeDelegated
        |> required "tag" D.string
        |> required "account" D.string
        |> required "baker" D.int


type alias EventBakerAdded =
    { tag : String
    , contents : Int
    }


eventBakerAddedDecoder : D.Decoder EventBakerAdded
eventBakerAddedDecoder =
    D.succeed EventBakerAdded
        |> required "tag" D.string
        |> required "contents" D.int


type AccountInfo
    = AddressAccount String
    | AddressContract String
    | AddressUnknown


accountInfoDecoder : D.Decoder AccountInfo
accountInfoDecoder =
    D.succeed
        (\tipe address ->
            case tipe of
                "AddressAccount" ->
                    AddressAccount address

                "AddressContract" ->
                    AddressContract address

                _ ->
                    AddressUnknown
        )
        |> required "type" D.string
        |> required "address" D.string


getBlockSummary_stub_allTransactions =
    """
{
  "specialEvents": [
    {
      "bakerid": 2,
      "rewardamount": 5610,
      "bakeraccount": "3fS4u95Sx9SKzU83kxxYP4SWaWirVix4T7P9bgswmceqvcs4vR"
    }
  ],
  "transactionSummaries": [
    {
      "hash": "15c2c8a0a9d496630dff603d4d404a6912d96215755884522798092bc179de5b",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 265,
      "result": {
        "events": [
          {
            "tag": "StakeDelegated",
            "account": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
            "baker": 0
          }
        ],
        "outcome": "success"
      },
      "energycost": 265,
      "type": "delegatestake",
      "index": 0
    },
    {
      "hash": "ae13d5677cdcbd90fad54e9768f4f1f4c44610c45af1c2ca0481119d47c8a2bb",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 265,
      "result": {
        "events": [
          {
            "tag": "StakeDelegated",
            "account": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
            "baker": 0
          }
        ],
        "outcome": "success"
      },
      "energycost": 265,
      "type": "delegatestake",
      "index": 1
    },
    {
      "hash": "24f8640636257c7dff13d510885da92116a5f68ba45e93bf900a007b16ecd39f",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 3167,
      "result": {
        "events": [
          {
            "tag": "BakerAdded",
            "contents": 5
          }
        ],
        "outcome": "success"
      },
      "energycost": 3167,
      "type": "addbaker",
      "index": 2
    },
    {
      "hash": "affc03c8211ea90cb72143e5c44eff7e55089668af8e64bd6b978ef7cfac39c7",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 265,
      "result": {
        "outcome": "reject",
        "rejectreason": {
          "tag": "InvalidStakeDelegationTarget",
          "contents": 12312312312
        }
      },
      "energycost": 265,
      "type": "delegatestake",
      "index": 3
    },
    {
      "hash": "9b1cf5312dab530fe8de8de7f120217687fe532744af73587ebe627d81e7e2cd",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 166,
      "result": {
        "outcome": "reject",
        "rejectreason": {
          "tag": "SerializationFailure"
        }
      },
      "energycost": 166,
      "type": null,
      "index": 4
    },
    {
      "hash": "8841d4dd1da2875b63d829be55fc5441adba2efa2baaed3b1228601e25bc2bb2",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 165,
      "result": {
        "outcome": "reject",
        "rejectreason": {
          "tag": "SerializationFailure"
        }
      },
      "energycost": 165,
      "type": null,
      "index": 5
    },
    {
      "hash": "5257c01e5f42a0afcead07149a474162c2a193702de59177bff0d7b8812a4d09",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 165,
      "result": {
        "outcome": "reject",
        "rejectreason": {
          "tag": "InvalidBakerRemoveSource",
          "contents": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt"
        }
      },
      "energycost": 165,
      "type": "removebaker",
      "index": 6
    }
  ]
}
"""


getBlockSummary_stub_failure =
    """
  {
    "specialEvents": [
      {
        "bakerid": 2,
        "rewardamount": 1751,
        "bakeraccount": "3fS4u95Sx9SKzU83kxxYP4SWaWirVix4T7P9bgswmceqvcs4vR"
      }
    ],
    "transactionSummaries": [
      {
        "hash": "0ada4e64558bb99a3607647c490f39083f44c709e654d689d8bf3fb343a95961",
        "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
        "cost": 165,
        "result": {
          "outcome": "reject",
          "rejectreason": {
            "tag": "InvalidAccountReference",
            "contents": "3Es2U5gMdKrqJdXSUVzgW4KUosJ91AxfsAuvKx5tKE9P2SvjVk"
          }
        },
        "energycost": 165,
        "type": "transfer",
        "index": 0
      }
    ]
  }
  """


getBlockSummaryResponseStub =
    """
{
  "specialEvents": [
    {
      "bakerid": 4,
      "rewardamount": 207,
      "bakeraccount": "4c7SXWg5bD3YpWQ2mmS8DSQiFm9MRpTd7Novo4ESPb1QLSYrts"
    }
  ],
  "transactionSummaries": [
    {
      "hash": "ae3154e1ee8a64d9e9718e57fbc979175be2ee8526b23fcddab2d626ebaeb72d",
      "sender": null,
      "cost": 0,
      "result": {
        "events": [
          {
            "tag": "AccountCreated",
            "contents": "3P2Qsi8FeMB6AijMi3kpD9FrcFfAkoHDMu8kf7Fbd3cYYJpk2s"
          },
          {
            "tag": "CredentialDeployed",
            "regid": "ac64aac1da15afd90d185475705935fbe224d5f7ddc43988e7db511656c787b452820b5096a2b323cf7612f5609d4cfb",
            "account": "3P2Qsi8FeMB6AijMi3kpD9FrcFfAkoHDMu8kf7Fbd3cYYJpk2s"
          }
        ],
        "outcome": "success"
      },
      "energycost": 35000,
      "type": null,
      "index": 0
    },
    {
      "hash": "2ecde309ea6b48cebfbb73eb1ee3364c4f10330955a4a13acb7d87076461ab2d",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "dba5e4bafc7d2201cc23d30f288e276b5305b0722a74d0ac1fe1f6b15d1496a2",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "f59897e5be69ecd5ed5843a10d74ee2b652064bd5077b853e6b324c23683af7b",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "f49899ac815b01a9f6e49c4d1fc926c511fcd3cc453c1307a504ef937a4fe8b5",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressContract"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "3ad7e0a0e3ed193bfba065c83704ae5bf84837255ac720565479afab81767c96",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressContract"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "d3b0cdeab57dc4d5e6b1faa1e4915ddfc2d025655775254e7124ae031688d9d8",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "bf6e33703977ce94caa918911f63b04654da0047454c484be9c80b487cc142c5",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "8781b249f4629f0ca9789fe143bfd4031208a7bfcca0ade9dd9f63ae8dac317d",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "41e6c23be80ab1151697ad16a4427268b0fd5440af0bb7bee4b330648669b164",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "71b1e81a467f038102b4028f62f1d75324b3f523472b89c583fdac918e762560",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "5298e7d571852da5c030c15b1513405c365111315efbc062a9d0f9082d1d5e6f",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "c7487dc3ee7cd097636badc9c21771d2a62a37052be8204078f516583cb5320e",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "dd6871001af51dc048bbae8ceb187c58fb2cf66ba838897b8f527642fb7e552a",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "fc2ee2cd1294f0dc8a0c738c25bc6f762edf2689ee37b22382d9a4b931637ae7",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "029681010cdc89383926d32017af7e51357f66b33f1527396f0af63b5b9a3e6c",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "64fbd260407f681292cc4931f8c3d7edb034574c172a90f1923188751fb6775a",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "5cc3ec13c9844456d8b65f0534dca943a4ece1b12c1ca0f8f5a2936c7e260722",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "5a9c8dd2b90b7943a4891b1ab41cbe8ae13cefb7a91c7ef3d884201408352b04",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "16672379084db2866db4564e0c42103d3f6edcdfedee6b9ce5f520b9aef78ea7",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    },
    {
      "hash": "de7eecba63d94c7810126cc325d6cf43d9382d8d2fed680202140fe97e91fe76",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energycost": 10,
      "type": "transfer"
    }
  ]
}
"""


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
