module Pages.Node exposing (nodeView, view)

import Dashboard.Formatting exposing (..)
import Dashboard.Widgets exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Events exposing (onClick)
import Element.Font as Font
import Html
import Html.Attributes exposing (style)
import NodeHelpers exposing (..)
import RemoteData exposing (RemoteData(..))
import Types exposing (..)


view : Model -> Element Msg
view model =
    content <|
        row [ paddingXY 0 20, spacing 20 ]
            [ case model.selectedNode of
                NotAsked ->
                    el [ alignTop ] (text "Loading...")

                Loading ->
                    el [ alignTop ] (text "Loading...")

                Failure err ->
                    el [ alignTop ] (text "Error loading node.")

                Success (Err nodeId) ->
                    el [ alignTop ] (text <| "Unknown node ID: " ++ nodeId ++ ".")

                Success (Ok node) ->
                    nodeView node model
            ]


nodeView : NetworkNode -> Model -> Element Msg
nodeView node model =
    let
        pairs =
            [ ( "Node name", el [ width (px 400) ] <| forceWrapTextElement node.nodeName )
            , ( "Node ID", text node.nodeId )
            , ( "Baker ID", text <| Maybe.withDefault "n/a" <| Maybe.map String.fromFloat node.consensusBakerId )
            , ( "Uptime", text <| asTimeAgoDuration node.uptime )
            , ( "Software version", text node.client )
            , ( "Average ping time", formatPing model.palette node.averagePing )
            , ( "Number of peers", text <| String.fromFloat node.peersCount )
            , ( "Best block", text node.bestBlock )
            , ( "Height of best block", text <| String.fromFloat node.bestBlockHeight )
            , ( "Arrive time of best block", text <| Maybe.withDefault "-" node.bestArrivedTime )
            , ( "Arrive period of best block (EMA)", text <| String.fromFloat <| Maybe.withDefault 0 node.blockArrivePeriodEMA )
            , ( "Arrive period of best block (EMSD)", text <| String.fromFloat <| Maybe.withDefault 0 node.blockArrivePeriodEMSD )
            , ( "Last finalized block", text node.finalizedBlock )
            , ( "Height of last finalized block", text <| String.fromFloat node.finalizedBlockHeight )
            , ( "Time of last finalization", text <| Maybe.withDefault "-" node.finalizedTime )
            , ( "Finalization period (EMA)", text <| String.fromFloat <| Maybe.withDefault 0 node.finalizationPeriodEMA )
            , ( "Finalization period (EMSD)", text <| String.fromFloat <| Maybe.withDefault 0 node.finalizationPeriodEMSD )
            , ( "Number of packets sent", text <| String.fromFloat node.packetsSent )
            , ( "Number of packets received", text <| String.fromFloat node.packetsReceived )
            , ( "Peers", remoteDataView model.palette (\nodes -> peersListView nodes node.peersList) model.nodes )
            ]

        statRows =
            pairs
                |> List.map
                    (\( label, elem ) ->
                        row [ height (shrink |> minimum 30) ]
                            [ column [ width (px 300), Font.color model.palette.fg1, alignTop ] [ text label ]
                            , column [ width fill, alignTop ] [ elem ]
                            ]
                    )
    in
    column [ Font.color model.palette.success ]
        statRows


forceWrapTextElement : String -> Element msg
forceWrapTextElement t =
    html <|
        Html.div
            [ style "overflow-wrap" "break-word"
            , style "white-space" "normal"
            , Html.Attributes.width 200
            ]
            [ Html.text t
            ]


peersListView : Dict Host NetworkNode -> List String -> Element Msg
peersListView nodes peersList =
    column [ width fill ]
        (List.map
            (\peerNodeId ->
                case findNodeById peerNodeId nodes of
                    Nothing ->
                        paragraph [] [ text <| "<" ++ peerNodeId ++ ">" ]

                    Just node ->
                        paragraph [ onClick <| NodeClicked peerNodeId, pointer ]
                            [ text <| node.nodeName
                            ]
            )
            peersList
        )
