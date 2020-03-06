module TypesDashboard exposing (..)

import Browser exposing (..)
import Browser.Navigation as Nav exposing (Key)
import Chain
import Dict exposing (..)
import Http
import Time
import Url exposing (Url)
import Url.Parser exposing (..)


type alias Model =
    { window : { width : Int, height : Int }
    , currentTime : Time.Posix
    , key : Key
    , currentPage : Page
    , nodes : Dict Host NetworkNode
    , sortMode : SortMode
    , selectedNode : Maybe NetworkNode
    , graph : { width : Float, height : Float }
    , chainModel : Chain.Model
    }


type Page
    = Dashboard
    | NodeGraph
    | NodeView String -- Node by nodeId
    | ChainViz


type alias Host =
    String


type Msg
    = CurrentTime Time.Posix
    | UrlClicked UrlRequest
    | UrlChanged Url
    | WindowResized Int Int
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
    , finalizationCommitteeMember : Bool
    }


type SortMode
    = SortAsc SortBy
    | SortDesc SortBy
    | SortNone


type SortBy
    = SortName
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


parserRoutes =
    oneOf
        [ Url.Parser.map Dashboard (s "")
        , Url.Parser.map NodeGraph (s "nodegraph")
        , Url.Parser.map NodeView (s "node" </> Url.Parser.string)
        , Url.Parser.map ChainViz (s "chain")
        ]


pathToPage : Url -> Page
pathToPage url =
    case parse parserRoutes url of
        Just page ->
            page

        Nothing ->
            Dashboard


pageToPath : Page -> String
pageToPath page =
    case page of
        Dashboard ->
            "/"

        NodeGraph ->
            "/nodegraph"

        NodeView nodeId ->
            "/node/" ++ nodeId

        ChainViz ->
            "/chain"
