port module Network exposing (..)

import Browser.Navigation as Nav exposing (Key)
import CollectionHelpers exposing (maxFrequency, median)
import Context exposing (Context)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (..)
import Formatting exposing (asSecondsAgo, averageStatSecondsFor)
import Http
import Icons
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Paging
import Palette
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route(..))
import ScrollHelpers exposing (scrollPageToTop)
import Time
import Widgets exposing (viewWidget)


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
    , bakingCommitteeMember : String
    , consensusBakerId : Maybe Float
    , finalizationCommitteeMember : Bool
    }


type alias Model =
    { nodes : WebData (Dict Host NetworkNode)
    , sortMode : SortMode
    , selectedNode : WebData (Result String NetworkNode)
    , nodesPagingModel : Paging.Model
    }


type Msg
    = NodeInfoReceived NetworkNode
    | FetchNodeSummaries Time.Posix
    | FetchedNodeSummaries (Result Http.Error (List NetworkNode))
    | SortSet SortBy
    | NodeClicked String
    | TaskPerformed
    | NodesPaging Paging.Msg


init : Model
init =
    { nodes = Loading
    , sortMode = SortNone
    , selectedNode = NotAsked
    , nodesPagingModel = Paging.init 50
    }


update : Msg -> Model -> Route -> Key -> ( Model, Cmd Msg )
update msg model currentRoute key =
    case msg of
        NodeInfoReceived node ->
            ( { model | nodes = RemoteData.map (Dict.insert node.nodeId node) model.nodes }, Cmd.none )

        FetchNodeSummaries _ ->
            ( model, Http.get { url = "/nodesSummary", expect = Http.expectJson FetchedNodeSummaries nodeSummariesDecoder } )

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
                            -- Update selected node to point to the new node model.
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
            ( { model | sortMode = newSortMode, nodesPagingModel = Paging.update Paging.FirstPage model.nodesPagingModel }, Cmd.none )

        NodeClicked nodeId ->
            ( selectNode model nodeId
            , Cmd.batch [ Nav.pushUrl key (Route.toString (NodeView nodeId)), scrollPageToTop TaskPerformed ]
            )

        TaskPerformed ->
            ( model, Cmd.none )

        NodesPaging pagingMsg ->
            ( { model | nodesPagingModel = Paging.update pagingMsg model.nodesPagingModel }, Cmd.none )


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
            |> required "bakingCommitteeMember" D.string
            |> required "consensusBakerId" (D.nullable D.float)
            |> optional "finalizationCommitteeMember" D.bool False
        )


findNodeById : String -> Dict Host NetworkNode -> Maybe NetworkNode
findNodeById nodeId =
    Dict.find (\_ n -> n.nodeId == nodeId) >> Maybe.map Tuple.second


viewSummaryWidgets : Context a -> WebData (Dict Host NetworkNode) -> Element msg
viewSummaryWidgets ctx remoteNodes =
    let
        iconSize =
            36
    in
    column [ spacing 12, width fill ]
        [ wrappedRow [ spacing 12, width fill ]
            (List.map (viewWidget ctx)
                [ { color = ctx.palette.c3
                  , title = "Active Nodes"
                  , description = ""
                  , icon = Icons.nodes iconSize
                  , value =
                        RemoteData.map
                            (\nodes ->
                                String.fromInt <| Dict.size <| nodePeersOnly nodes
                            )
                            remoteNodes
                  , subvalue = Nothing
                  }
                , { color = ctx.palette.c1
                  , title = "Last Block"
                  , description = ""
                  , icon = Icons.lastBlock iconSize
                  , value =
                        RemoteData.map
                            (\nodesDict ->
                                let
                                    compatibleNodes =
                                        Dict.filter (\_ -> allowedNodeVersion ctx) nodesDict
                                in
                                majorityStatFor
                                    (\node ->
                                        Maybe.withDefault "" node.bestArrivedTime
                                            |> Iso8601.toTime
                                            |> Result.toMaybe
                                            |> asSecondsAgo ctx.time
                                    )
                                    ""
                                    compatibleNodes
                            )
                            remoteNodes
                  , subvalue = Nothing
                  }
                , { color = ctx.palette.c2
                  , title = "Last Finalization"
                  , description = ""
                  , icon = Icons.lastFinalizedBlock iconSize
                  , value =
                        RemoteData.map
                            (\nodeDict ->
                                let
                                    nodes =
                                        Dict.values nodeDict
                                            |> List.filter (allowedNodeVersion ctx)
                                in
                                nodes
                                    |> List.map .finalizedBlockHeight
                                    |> maxFrequency
                                    |> Maybe.andThen
                                        (\height ->
                                            List.filter (\node -> node.finalizedBlockHeight == height) nodes
                                                |> List.filterMap .finalizedTime
                                                |> List.filterMap (Iso8601.toTime >> Result.toMaybe)
                                                |> List.map Time.posixToMillis
                                                |> List.sort
                                                |> median
                                        )
                                    |> Maybe.map Time.millisToPosix
                                    |> asSecondsAgo ctx.time
                            )
                            remoteNodes
                  , subvalue = Nothing
                  }
                , { color = ctx.palette.c1
                  , title = "Total Length"
                  , description = ""
                  , icon =
                        Icons.chainLength iconSize
                            (Palette.uiToColor ctx.palette.c1)
                            (Palette.uiToColor ctx.palette.c2)
                  , value =
                        RemoteData.map
                            (\nodesDict ->
                                let
                                    compatibleNodes =
                                        Dict.filter (\_ -> allowedNodeVersion ctx) nodesDict
                                in
                                String.fromInt <| round (majorityStatFor .bestBlockHeight -1 compatibleNodes)
                            )
                            remoteNodes
                  , subvalue = Nothing
                  }
                , { color = ctx.palette.c2
                  , title = "Finalized Length"
                  , description = ""
                  , icon =
                        Icons.finalizedLength iconSize
                            (Palette.uiToColor ctx.palette.c1)
                            (Palette.uiToColor ctx.palette.c2)
                  , value =
                        RemoteData.map
                            (\nodesDict ->
                                let
                                    compatibleNodes =
                                        Dict.filter (\_ -> allowedNodeVersion ctx) nodesDict
                                in
                                String.fromInt <| round (majorityStatFor .finalizedBlockHeight -1 compatibleNodes)
                            )
                            remoteNodes
                  , subvalue = Nothing
                  }
                ]
            )
        , wrappedRow [ spacing 12, width fill ]
            (List.map (viewWidget ctx)
                [ { color = ctx.palette.c1
                  , title = "Block Time"
                  , description = "Average time between verified blocks"
                  , icon = Icons.lastBlockEMA iconSize
                  , value =
                        RemoteData.map
                            (\nodesDict ->
                                let
                                    compatibleNodes =
                                        Dict.filter (\_ -> allowedNodeVersion ctx) nodesDict
                                in
                                averageStatSecondsFor .blockArrivePeriodEMA compatibleNodes
                            )
                            remoteNodes
                  , subvalue =
                        Nothing
                  }
                , { color = ctx.palette.c2
                  , title = "Finalization Time"
                  , description = "Average time between completed finalizations"
                  , icon = Icons.lastFinalizedBlockEMA iconSize
                  , value =
                        RemoteData.map
                            (\nodesDict ->
                                let
                                    compatibleNodes =
                                        Dict.filter (\_ -> allowedNodeVersion ctx) nodesDict
                                in
                                averageStatSecondsFor .finalizationPeriodEMA compatibleNodes
                            )
                            remoteNodes
                  , subvalue =
                        Nothing
                  }
                ]
            )
        ]


nodePeersOnly : Dict Host NetworkNode -> Dict Host NetworkNode
nodePeersOnly nodes =
    nodes |> Dict.filter (\_ n -> n.peerType == "Node")


{-| Check whether the client version is recent enough.
NB: This will only work until we have node version 10, after that it needs to be revised.
-}
allowedNodeVersion : { ctx | minVersionIncludedInStats : String } -> { a | client : String } -> Bool
allowedNodeVersion ctx node =
    node.client >= ctx.minVersionIncludedInStats


{-| For the given node attribute, finds majority value across all nodes
and returns that, or the default if unknown.
-}
majorityStatFor :
    (a -> comparable)
    -> comparable
    -> Dict b a
    -> comparable
majorityStatFor getter default nodes =
    nodes
        |> Dict.values
        |> List.map getter
        |> maxFrequency
        |> Maybe.withDefault default
