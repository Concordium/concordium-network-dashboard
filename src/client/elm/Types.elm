module Types exposing (..)

import Browser exposing (..)
import Browser.Navigation as Nav exposing (Key)
import Chain
import Dict exposing (..)
import Element
import Explorer
import Http
import Json.Decode as D
import Palette exposing (ColorMode, Palette)
import RemoteData exposing (WebData)
import Route exposing (Route)
import Time
import Url exposing (Url)


type alias Model =
    { key : Key
    , time : Time.Posix
    , window : { width : Int, height : Int }
    , palette : Palette Element.Color
    , colorMode : ColorMode
    , currentRoute : Route
    , nodes : WebData (Dict Host NetworkNode)
    , sortMode : SortMode
    , selectedNode : Maybe NetworkNode
    , graph : { width : Float, height : Float }
    , chainModel : Chain.Model
    , explorerModel : Explorer.Model
    }


type alias Host =
    String


type Msg
    = CurrentTime Time.Posix
    | UrlClicked UrlRequest
    | UrlChanged Url
    | WindowResized Int Int
    | CopyToClipboard String
    | StorageDocReceived D.Value
      --
    | NodeInfoReceived NetworkNode
    | FetchNodeSummaries Time.Posix
    | FetchedNodeSummaries (Result Http.Error (List NetworkNode))
    | SortSet SortBy
    | NodeClicked String
    | GraphZoom Float
    | DevResetCache
    | NoopHttp (Result Http.Error ())
    | ChainMsg Chain.Msg
    | BlockSelected String
    | ToggleDarkMode
    | ExplorerMsg Explorer.Msg
      -- Debug
    | Noop


type alias NetworkNode =
    { nodeName : String
    , nodeId : String
    , peerType : String

    --   , state : Maybe String
    , uptime : Float -- Milliseconds @TODO figure out how to convert to Int, issue is in JS everything is Double even Ints
    , client : String
    , averagePing : Maybe Float -- Milliseconds @TODO as above figure out Int. Maybe for when 0 nodes
    , peersCount : Float -- @TODO as above figure out Int
    , peersList : List String
    , bestBlock : String
    , bestBlockHeight : Float
    , bestArrivedTime : Maybe String
    , blockArrivePeriodEMA : Maybe Float
    , blockArrivePeriodEMSD : Maybe Float
    , finalizedBlock : String
    , finalizedBlockHeight : Float
    , finalizedTime : Maybe String
    , finalizationPeriodEMA : Maybe Float
    , finalizationPeriodEMSD : Maybe Float
    , packetsSent : Float -- @TODO as above figure out Int
    , packetsReceived : Float -- @TODO as above figure out Int
    , consensusRunning : Bool
    , bakingCommitteeMember : Bool
    , consensusBakerId : Maybe Float
    , finalizationCommitteeMember : Bool
    }


type SortMode
    = SortAsc SortBy
    | SortDesc SortBy
    | SortNone


type SortBy
    = SortName
    | SortBaker
    | SortUptime
    | SortClient
    | SortAvgPing
    | SortPeers
    | SortSent
    | SortReceived
    | SortBlock
    | SortHeight
    | SortFinalizedBlock
    | SortFinalizedHeight
