module Explorer.Request exposing (..)

import Config
import Http exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (..)
import Task



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


getBlockSummaryStub_ =
    case D.decodeString blockSummaryDecoder getBlockSummaryResponseStub of
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
    , blockArriveTime : String
    , blockReceiveTime : String
    , transactionCount : Int
    , transactionEnergyCost : Int
    , blockSlot : Int
    , blockLastFinalized : String
    , blockSlotTime : String
    , blockHeight : Int
    , blockBaker : Int
    , executionCost : Int
    , centralBankAmount : Int
    }


blockInfoStub : BlockInfo
blockInfoStub =
    { transactionsSize = 0
    , blockParent = "STUB"
    , mintedAmountPerSlot = 100
    , totalEncryptedAmount = 0
    , blockHash = "STUB"
    , finalized = True
    , totalAmount = 15000628024800
    , blockArriveTime = "2020-03-05T17:04:10.8763399Z"
    , blockReceiveTime = "2020-03-05T17:04:10.8763399Z"
    , transactionCount = 0
    , transactionEnergyCost = 0
    , blockSlot = 6280248
    , blockLastFinalized = "a0cdd5b7e51d83bef2d0ed55cb35cdc8c42280add22e9e59c893ecb492ff609a"
    , blockSlotTime = "2020-03-05T16:08:00Z"
    , blockHeight = 79
    , blockBaker = 2
    , executionCost = 0
    , centralBankAmount = 474
    }


blockInfoDecoder =
    D.succeed BlockInfo
        |> required "transactionsSize" D.int
        |> required "blockParent" D.string
        |> required "mintedAmountPerSlot" D.int
        |> required "totalEncryptedAmount" D.int
        |> required "blockHash" D.string
        |> required "finalized" D.bool
        |> required "totalAmount" D.int
        |> required "blockArriveTime" D.string
        |> required "blockReceiveTime" D.string
        |> required "transactionCount" D.int
        |> required "transactionEnergyCost" D.int
        |> required "blockSlot" D.int
        |> required "blockLastFinalized" D.string
        |> required "blockSlotTime" D.string
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


blockSummaryDecoder =
    D.succeed BlockSummary
        |> required "specialEvents" (D.list specialEventDecoder)
        |> required "transactionSummaries" (D.list transactionSummaryDecoder)


type alias SpecialEvent =
    { bakerid : Int
    , rewardamount : Int
    , bakeraccount : String
    }


specialEventDecoder =
    D.succeed SpecialEvent
        |> required "bakerid" D.int
        |> required "rewardamount" D.int
        |> required "bakeraccount" D.string


type alias TransactionSummary =
    { hash : String
    , sender : String
    , cost : Int
    , events : List TransactionEvent
    , energycost : Int
    , tipe : String
    }


transactionSummaryDecoder =
    D.succeed TransactionSummary
        |> required "hash" D.string
        |> required "sender" D.string
        |> required "cost" D.int
        |> required "result" (D.field "events" (D.list transactionEventDecoder))
        |> required "energycost" D.int
        |> required "type" D.string


type alias TransactionEvent =
    { amount : Int
    , tag : String
    , to : AccountInfo
    , from : AccountInfo
    }


transactionEventDecoder =
    D.succeed TransactionEvent
        |> required "amount" D.int
        |> required "tag" D.string
        |> required "to" accountInfoDecoder
        |> required "from" accountInfoDecoder


type AccountInfo
    = AddressAccount String
    | AddressContract String
    | AddressUnknown


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
