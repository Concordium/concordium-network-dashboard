module Pages.Graph exposing (nodeView, view)

import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import NetworkGraph
import Types exposing (..)
import Widgets exposing (..)


view model =
    [ header
    , row [ paddingXY 0 20, spacing 20 ]
        [ column
            [ height (px 800)
            , width (px 800)
            , Background.color moduleGrey
            , Border.rounded 5
            ]
            [ html <| NetworkGraph.agedRelations model model.nodes
            , row [ spacing 5 ]
                [ el [ padding 10, onClick (GraphZoom 100) ] (text "Zoom Out")
                , el [ padding 10, onClick (GraphZoom -100) ] (text "Zoom In")
                ]
            ]
        , case model.selectedNode of
            Just node ->
                nodeView node model

            Nothing ->
                el [ alignTop ] (text "Click on a node to see an overview")
        ]
    ]


nodeView node model =
    let
        pairs =
            [ ( "nodeName", text node.nodeName )
            , ( "nodeId", text node.nodeId )
            , ( "uptime", text <| asTimeAgoDuration node.uptime )
            , ( "client", text node.client )
            , ( "averagePing", formatPing node.averagePing )
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
            , ( "peersList", column [] (List.map (\t -> paragraph [] [ text t ]) node.peersList) )
            ]
    in
    row [ Font.color green, spacing 20, alignTop ]
        [ column [ Font.color lightGrey, alignTop ] (pairs |> List.map Tuple.first |> List.map (\t -> el [ height (px 30) ] (text t)))
        , column [ alignTop ] (pairs |> List.map Tuple.second |> List.map (\e -> el [ height (px 30) ] e))
        ]
