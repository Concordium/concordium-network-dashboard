port module Network exposing (..)

import Browser.Navigation as Nav exposing (Key)
import Config
import Dict exposing (Dict)
import Dict.Extra as Dict
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route(..))
import ScrollHelpers exposing (scrollPageToTop)
import Time


port nodeInfo : (NetworkNode -> msg) -> Sub msg


type alias Host =
    String


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


type alias NetworkNode =
    { nodeName : String
    , nodeId : String
    , peerType : String
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


type alias Model =
    { nodes : WebData (Dict Host NetworkNode)
    , sortMode : SortMode
    , selectedNode : WebData (Result String NetworkNode)
    }


type Msg
    = NodeInfoReceived NetworkNode
    | FetchNodeSummaries Time.Posix
    | FetchedNodeSummaries (Result Http.Error (List NetworkNode))
    | SortSet SortBy
    | NodeClicked String
    | TaskPerformed


init : Model
init =
    { nodes = Loading
    , sortMode = SortNone
    , selectedNode = NotAsked
    }


update : Msg -> Model -> Route -> Key -> ( Model, Cmd Msg )
update msg model currentRoute key =
    case msg of
        NodeInfoReceived node ->
            ( { model | nodes = RemoteData.map (Dict.insert node.nodeId node) model.nodes }, Cmd.none )

        FetchNodeSummaries _ ->
            ( model, Http.get { url = Config.collector ++ "/nodesSummary", expect = Http.expectJson FetchedNodeSummaries nodeSummariesDecoder } )

        FetchedNodeSummaries r ->
            case r of
                Ok nodeSummaries ->
                    let
                        nodes =
                            nodeSummaries
                                |> List.map (\node -> ( node.nodeId, node ))
                                |> Dict.fromList

                        newModel =
                            { model
                                | nodes = Success nodes
                            }
                    in
                    case currentRoute of
                        NodeView nodeId ->
                            -- Reinitialize selected node becuase nodeSummaries may have loaded after a nodeview URL was already open.
                            ( selectNode newModel nodeId, Cmd.none )

                        _ ->
                            ( newModel, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        SortSet sortBy ->
            let
                newSortMode =
                    case model.sortMode of
                        SortNone ->
                            SortAsc sortBy

                        SortAsc sortBy_ ->
                            if sortBy_ == sortBy then
                                SortDesc sortBy

                            else
                                SortAsc sortBy

                        SortDesc sortBy_ ->
                            if sortBy_ == sortBy then
                                SortNone

                            else
                                SortAsc sortBy
            in
            ( { model | sortMode = newSortMode }, Cmd.none )

        NodeClicked nodeId ->
            ( selectNode model nodeId
            , Cmd.batch [ Nav.pushUrl key (Route.toString (NodeView nodeId)), scrollPageToTop TaskPerformed ]
            )

        TaskPerformed ->
            ( model, Cmd.none )


selectNode : Model -> String -> Model
selectNode model nodeId =
    { model | selectedNode = RemoteData.map (findNodeById nodeId >> Result.fromMaybe nodeId) model.nodes }


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ nodeInfo NodeInfoReceived
        , Time.every 2000 FetchNodeSummaries
        ]


nodeSummariesDecoder : D.Decoder (List NetworkNode)
nodeSummariesDecoder =
    D.list
        (D.succeed NetworkNode
            |> required "nodeName" D.string
            |> required "nodeId" D.string
            -- @TODO make this mandatory when collector has been deployed
            |> optional "peerType" D.string "Unknown"
            |> required "uptime" D.float
            |> required "client" D.string
            |> required "averagePing" (D.nullable D.float)
            |> required "peersCount" D.float
            |> required "peersList" (D.list D.string)
            |> required "bestBlock" D.string
            |> required "bestBlockHeight" D.float
            |> required "bestArrivedTime" (D.nullable D.string)
            |> required "blockArrivePeriodEMA" (D.nullable D.float)
            |> required "blockArrivePeriodEMSD" (D.nullable D.float)
            |> required "finalizedBlock" D.string
            |> required "finalizedBlockHeight" D.float
            |> required "finalizedTime" (D.nullable D.string)
            |> required "finalizationPeriodEMA" (D.nullable D.float)
            |> required "finalizationPeriodEMSD" (D.nullable D.float)
            |> required "packetsSent" D.float
            |> required "packetsReceived" D.float
            |> optional "consensusRunning" D.bool False
            |> optional "bakingCommitteeMember" D.bool False
            |> required "consensusBakerId" (D.nullable D.float)
            |> optional "finalizationCommitteeMember" D.bool False
        )


findNodeById : String -> Dict Host NetworkNode -> Maybe NetworkNode
findNodeById nodeId =
    Dict.find (\_ n -> n.nodeId == nodeId) >> Maybe.map Tuple.second
