module Pages.Graph exposing (nodeView, view)

-- import NetworkGraph

import Dashboard.Formatting exposing (..)
import Dashboard.Widgets exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes exposing (style)
import NodeHelpers exposing (..)
import Types exposing (..)


view : Model -> Element Msg
view model =
    content <|
        row [ paddingXY 0 20, spacing 20 ]
            [ -- networkGraph model
              case model.selectedNode of
                Just node ->
                    nodeView node model

                Nothing ->
                    case model.currentPage of
                        NodeView _ ->
                            el [ alignTop ] (text "Loading...")

                        _ ->
                            el [ alignTop ] (text "Click on a node to see an overview")
            ]


networkGraph model =
    column
        [ height (px 800)
        , width (px 800)
        , Background.color model.palette.bg2
        , Border.rounded 5
        , alignTop
        ]
        [ --html <| NetworkGraph.agedRelations model model.nodes,
          row [ spacing 5 ]
            [ el [ padding 10, onClick (GraphZoom 100) ] (text "Zoom Out")
            , el [ padding 10, onClick (GraphZoom -100) ] (text "Zoom In")
            ]
        ]


nodeView : NetworkNode -> Model -> Element Msg
nodeView node model =
    let
        pairs =
            [ ( "nodeName", el [ width (px 400) ] <| forceWrapTextElement node.nodeName )
            , ( "nodeId", text node.nodeId )
            , ( "bakerId", text <| Maybe.withDefault "n/a" <| Maybe.map String.fromFloat node.consensusBakerId )
            , ( "uptime", text <| asTimeAgoDuration node.uptime )
            , ( "client", text node.client )
            , ( "averagePing", formatPing model.palette node.averagePing )
            , ( "peersCount", text <| String.fromFloat node.peersCount )
            , ( "bestBlock", text node.bestBlock )
            , ( "bestBlockHeight", text <| String.fromFloat node.bestBlockHeight )
            , ( "bestArrivedTime", text <| Maybe.withDefault "-" node.bestArrivedTime )
            , ( "blockArrivePeriodEMA", text <| String.fromFloat <| Maybe.withDefault 0 node.blockArrivePeriodEMA )
            , ( "blockArrivePeriodEMSD", text <| String.fromFloat <| Maybe.withDefault 0 node.blockArrivePeriodEMSD )
            , ( "finalizedBlock", text node.finalizedBlock )
            , ( "finalizedBlockHeight", text <| String.fromFloat node.finalizedBlockHeight )
            , ( "finalizedTime", text <| Maybe.withDefault "-" node.finalizedTime )
            , ( "finalizationPeriodEMA", text <| String.fromFloat <| Maybe.withDefault 0 node.finalizationPeriodEMA )
            , ( "finalizationPeriodEMSD", text <| String.fromFloat <| Maybe.withDefault 0 node.finalizationPeriodEMSD )
            , ( "packetsSent", text <| String.fromFloat node.packetsSent )
            , ( "packetsReceived", text <| String.fromFloat node.packetsReceived )
            , ( "peersList", remoteDataView model.palette (\nodes -> peersListView nodes node.peersList) model.nodes )
            ]

        statRows =
            pairs
                |> List.map
                    (\( label, elem ) ->
                        row [ height (shrink |> minimum 30) ]
                            [ column [ width (px 200), Font.color model.palette.fg1 ] [ text label ]
                            , column [ width fill ] [ elem ]
                            ]
                    )
    in
    column [ Font.color model.palette.success, alignTop ]
        statRows


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
                paragraph [ onClick <| NodeClicked peerNodeId, pointer ]
                    [ text <| "(" ++ nodeTypeById peerNodeId nodes ++ ") " ++ peerNodeId ]
            )
            peersList
        )
