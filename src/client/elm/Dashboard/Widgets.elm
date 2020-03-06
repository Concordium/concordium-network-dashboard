module Dashboard.Widgets exposing (..)

import ColorsDashboard exposing (..)
import Dashboard.Formatting exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Round
import Trend.Math
import TypesDashboard exposing (..)


content : Element msg -> Element msg
content e =
    el [ width fill, height fill, paddingXY 30 0 ] e


summaryWidgets : Model -> Element msg
summaryWidgets model =
    column [ spacing 20, width fill ]
        [ wrappedRow [ spacing 20, width fill ]
            [ widgetNumber purple
                "Active Nodes"
                "/assets/images/icon-nodes-purple.png"
                (toFloat <| Dict.size <| nodePeersOnly model.nodes)
            , widgetSeconds blue
                "Last Block"
                "/assets/images/icon-lastblock-lightblue.png"
                (majorityStatFor
                    (\n ->
                        asSecondsAgo
                            model.currentTime
                            (Maybe.withDefault "" n.bestArrivedTime)
                    )
                    ""
                    model.nodes
                )
            , widgetSeconds green
                "Last finalized block"
                "/assets/images/icon-blocklastfinal-green.png"
                -- Take the highest finalised block height and then the oldest (smallest) time of those
                (asSecondsAgo model.currentTime
                    (Maybe.withDefault ""
                        (withinHighestStatFor
                            .finalizedBlockHeight
                            ""
                            model.nodes
                            .finalizedTime
                        )
                    )
                )
            , widgetNumber blue
                "Chain Len."
                "/assets/images/icon-blocks-blue.png"
                (majorityStatFor .bestBlockHeight -1 model.nodes)
            , widgetNumber green
                "Finalized Len."
                "/assets/images/icon-blocksfinal-green.png"
                (majorityStatFor .finalizedBlockHeight -1 model.nodes)
            ]
        , wrappedRow [ spacing 20, width fill ]
            [ widgetTextSub pink
                "Last Block EMA"
                "/assets/images/icon-rocket-pink.png"
                (averageStatSecondsFor .blockArrivePeriodEMA model.nodes)
                (model.nodes
                    |> Dict.toList
                    |> List.map Tuple.second
                    |> List.map .blockArrivePeriodEMA
                    |> justs
                    |> Trend.Math.stddev
                    |> Result.map (Round.round 2)
                    |> Result.withDefault "-"
                    |> (++) "stddev: "
                )
            , widgetTextSub pink
                "Last Finalization EMA"
                "/assets/images/icon-rocket-pink.png"
                (averageStatSecondsFor .finalizationPeriodEMA model.nodes)
                (model.nodes
                    |> Dict.toList
                    |> List.map Tuple.second
                    |> List.map .blockArrivePeriodEMA
                    |> justs
                    |> Trend.Math.stddev
                    |> Result.map (Round.round 2)
                    |> Result.withDefault "-"
                    |> (++) "stddev: "
                )

            -- , worldMap
            -- , chartTimeseries blue "Active Nodes" "/assets/images/icon-blocks-blue.png" (Dict.size model.nodes)
            ]
        ]


widgetsForWebsite : Model -> List (Element msg)
widgetsForWebsite model =
    [ widgetNumber purple
        "Active Nodes"
        "/assets/images/icon-nodes-purple.png"
        (toFloat <| Dict.size model.nodes)
    , widgetSeconds lightBlue
        "Last Block"
        "/assets/images/icon-lastblock-lightblue.png"
        (majorityStatFor
            (\n ->
                asSecondsAgo model.currentTime (Maybe.withDefault "" n.bestArrivedTime)
            )
            ""
            model.nodes
        )

    -- , widgetSeconds green "Last finalized block" "/assets/images/icon-blocklastfinal-green.png" (majorityStatFor (\n -> asSecondsAgo model.currentTime (Maybe.withDefault "" n.finalizedTime)) -1 model.nodes)
    , widgetNumber blue
        "Block Height"
        "/assets/images/icon-blocks-blue.png"
        (majorityStatFor .bestBlockHeight -1 model.nodes)

    -- , widgetNumber green "Finalized height" "/assets/images/icon-blocksfinal-green.png" (majorityStatFor .finalizedBlockHeight -1 model.nodes)
    , widgetTextSub pink
        "Last Block EMA"
        "/assets/images/icon-rocket-pink.png"
        (averageStatSecondsFor .blockArrivePeriodEMA model.nodes)
        (model.nodes
            |> Dict.toList
            |> List.map Tuple.second
            |> List.map .blockArrivePeriodEMA
            |> justs
            |> Trend.Math.stddev
            |> Result.map (Round.round 2)
            |> Result.withDefault "-"
            |> (++) "stddev: "
        )

    -- , widgetText pink "Avg Finalization Time" "/assets/images/icon-rocket-pink.png" <|
    --     averageStatSecondsFor .finalizationPeriodEMA model.nodes
    ]


widgetText : Color -> String -> String -> String -> Element msg
widgetText color title icon value =
    row
        [ height (px 120)
        , width (fillPortion 1)
        , Background.color moduleGrey
        , padding 20
        , spacing 20
        , Border.rounded 5
        ]
        [ column []
            [ row
                [ Background.color darkGrey
                , Border.rounded 100
                , height (px 70)
                , width (px 70)
                ]
                [ image [ height (px 35), centerY, centerX ] { src = icon, description = "Decorative icon" } ]
            ]
        , column [ spacing 12 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ]
                [ text value
                ]
            ]
        ]


widgetTextSub : Color -> String -> String -> String -> String -> Element msg
widgetTextSub color title icon value subvalue =
    row
        [ height (px 120)
        , width (fillPortion 1)
        , paddingXY 28 20
        , Background.color moduleGrey
        , spacing 20
        , Border.rounded 5
        ]
        [ column []
            [ row
                [ Background.color darkGrey
                , Border.rounded 100
                , height (px 70)
                , width (px 70)
                ]
                [ image [ height (px 35), centerY, centerX ] { src = icon, description = "Decorative icon" } ]
            ]
        , column [ spacing 12 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , column [ Font.color color, Font.size 30 ]
                [ text value
                ]
            , if value == "-" then
                none

              else
                column [ Font.color color ] [ text subvalue ]
            ]
        ]


widgetSeconds : Color -> String -> String -> String -> Element msg
widgetSeconds color title icon value =
    row
        [ height (px 120)
        , width (fillPortion 1)
        , Background.color moduleGrey
        , padding 20
        , spacing 20
        , Border.rounded 5
        ]
        [ column []
            [ row
                [ Background.color darkGrey
                , Border.rounded 100
                , height (px 70)
                , width (px 70)
                ]
                [ image [ height (px 35), centerY, centerX ]
                    { src = icon, description = "Decorative icon" }
                ]
            ]
        , column [ spacing 12 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ] [ text <| value ]
            ]
        ]


widgetNumber : Color -> String -> String -> Float -> Element msg
widgetNumber color title icon value =
    row
        [ height (px 120)
        , width (fillPortion 1)
        , Background.color moduleGrey
        , padding 20
        , spacing 20
        , Border.rounded 5
        ]
        [ column []
            [ row
                [ Background.color darkGrey
                , Border.rounded 100
                , height (px 70)
                , width (px 70)
                ]
                [ image [ height (px 35), centerY, centerX ]
                    { src = icon, description = "Decorative icon" }
                ]
            ]
        , column [ spacing 12 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ]
                [ text <|
                    if value >= 0 then
                        String.fromFloat value

                    else
                        "-"
                ]
            ]
        ]



-- widgetNumberChart color title icon value =
--     row [ Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
--         [ column []
--             [ row [ Background.color darkGrey, Border.rounded 100, height (px 70), width (px 70) ] [ image [ height (px 40), centerY, centerX ] { src = icon, description = "Decorative icon" } ] ]
--         , column [ spacing 20 ]
--             [ row [ Font.color color ] [ text <| String.toUpper title ]
--             , row [ Font.color color, Font.size 30 ]
--                 [ text <|
--                     if value >= 0 then
--                         String.fromFloat value
--
--                     else
--                         "-"
--                 ]
--             ]
--         , column [ width (px 200) ]
--             [ html Chart.test ]
--         ]
--
--
-- chartTimeseries color title icon value =
--     row [ Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
--         -- @TODO play with this later to finish the chart effect
--         -- Background.gradient { angle = pi, steps = [ rgba255 49 178 239 1, rgba255 0 0 0 0 ] }
--         [ column [ spacing 20 ]
--             [ row [ Font.color color ] [ text <| String.toUpper title ]
--             , row [ Font.color color, Font.size 30 ] [ text <| String.fromFloat value ]
--             ]
--         , column [ width (px 200) ]
--             [ html Chart.test ]
--         ]


worldMap : Element msg
worldMap =
    row [ height (px 260), width (fillPortion 1), Background.color moduleGrey, Border.rounded 5 ]
        [ column [ spacing 0 ]
            [ row [ Font.color blue, paddingXY 20 0 ] [ text <| String.toUpper "Node Locations" ]
            , image [ height (px 220), centerY, centerX ] { src = "/assets/images/world.svg", description = "World Map" }
            ]
        ]


nodesTable : Model -> List NetworkNode -> Element Msg
nodesTable model nodes =
    if List.length nodes == 0 then
        row [ Font.color green ] [ text "Waiting for node statistics..." ]

    else
        Element.table [ spacing 12, Font.color green, alignTop, width fill ]
            { data = nodes
            , columns =
                [ { header = sortableHeader model SortName "Name"
                  , width = fill
                  , view =
                        \node ->
                            el
                                [ pointer
                                , onClick (NodeClicked node.nodeId)
                                ]
                                (text <| ellipsis 30 node.nodeName)
                  }

                --, { header = text "State"
                --  , width = fill
                --, view =
                --      \node ->
                --           text <| Maybe.withDefault "<No state loaded>" node.state
                --}
                , { header = sortableHeader model SortUptime "Uptime"
                  , width = fill
                  , view =
                        \node ->
                            text <| asTimeAgoDuration node.uptime
                  }
                , { header = sortableHeader model SortClient "Client"
                  , width = fill
                  , view =
                        \node ->
                            text node.client
                  }
                , { header = sortableHeader model SortAvgPing "Avg Ping"
                  , width = fill
                  , view =
                        \node -> formatPing node.averagePing
                  }
                , { header = sortableHeader model SortPeers "Peers"
                  , width = fill
                  , view =
                        \node ->
                            if node.peersCount == 0 then
                                el [ Font.color red ] (text "0")

                            else
                                text <| String.fromFloat node.peersCount
                  }
                , { header = sortableHeader model SortSent "Sent"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.packetsSent
                  }
                , { header = sortableHeader model SortReceived "Received"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.packetsReceived
                  }
                , { header = sortableHeader model SortBlock "Block"
                  , width = fill
                  , view =
                        \node ->
                            text <| hashSnippet node.bestBlock
                  }
                , { header = sortableHeader model SortHeight "Block Height"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.bestBlockHeight
                  }
                , { header = sortableHeader model SortFinalizedBlock "Finalized Block"
                  , width = fill
                  , view =
                        \node ->
                            text <| hashSnippet node.finalizedBlock
                  }
                , { header = sortableHeader model SortFinalizedHeight "Finalized Height"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.finalizedBlockHeight
                  }
                , { header = text "Finalized Time"
                  , width = fill
                  , view =
                        \node ->
                            text <| asSecondsAgo model.currentTime (Maybe.withDefault "" node.finalizedTime)
                  }
                , { header = text "Last Block EMA"
                  , width = fill
                  , view =
                        \node ->
                            text <| Round.round 2 <| Maybe.withDefault 0 node.blockArrivePeriodEMA
                  }
                , { header = text "Last Finalization EMA"
                  , width = fill
                  , view =
                        \node ->
                            text <| Round.round 2 <| Maybe.withDefault 0 node.finalizationPeriodEMA
                  }
                ]
            }


hashSnippet hash =
    String.left 6 hash ++ "..."


sortableHeader model sortBy name =
    let
        withIcon url =
            row [ spacing 5, Font.color lightGrey, pointer ]
                [ el [ onClick <| SortSet sortBy ] (text name)
                , image [ width (px 10) ] { src = url, description = "Sort Ascending Icon" }
                ]

        withoutIcon =
            el [ onClick <| SortSet sortBy, Font.color lightGrey, pointer ] (text name)
    in
    case model.sortMode of
        SortAsc sortBy_ ->
            if sortBy_ == sortBy then
                withIcon "/assets/images/icon-arrow-up.png"

            else
                withoutIcon

        SortDesc sortBy_ ->
            if sortBy_ == sortBy then
                withIcon "/assets/images/icon-arrow-down.png"

            else
                withoutIcon

        SortNone ->
            withoutIcon


sortNodesMode : SortMode -> List NetworkNode -> List NetworkNode
sortNodesMode sortMode listNodes =
    case sortMode of
        SortAsc sortBy ->
            sortNodesBy sortBy listNodes

        SortDesc sortBy ->
            sortNodesBy sortBy listNodes |> List.reverse

        SortNone ->
            listNodes


sortNodesBy sortBy listNodes =
    case sortBy of
        SortName ->
            List.sortBy .nodeName listNodes

        SortUptime ->
            List.sortBy .uptime listNodes

        SortClient ->
            List.sortBy .client listNodes

        SortAvgPing ->
            List.sortBy
                (\n ->
                    case n.averagePing of
                        Nothing ->
                            -- Sort n/a's to 'bottom' by giving them a large number
                            1000000000

                        Just x ->
                            x
                )
                listNodes

        SortPeers ->
            List.sortBy .peersCount listNodes

        SortSent ->
            List.sortBy .packetsSent listNodes

        SortReceived ->
            List.sortBy .packetsReceived listNodes

        SortBlock ->
            List.sortBy .bestBlock listNodes

        SortHeight ->
            List.sortBy .bestBlockHeight listNodes

        SortFinalizedBlock ->
            List.sortBy .finalizedBlock listNodes

        SortFinalizedHeight ->
            List.sortBy .finalizedBlockHeight listNodes


nodePeersOnly : Dict Host NetworkNode -> Dict Host NetworkNode
nodePeersOnly nodes =
    -- @TODO remove "" case when new collector is deployed
    nodes |> Dict.filter (\k n -> n.peerType == "Node" || n.peerType == "")
