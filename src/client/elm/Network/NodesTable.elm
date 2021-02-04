module Network.NodesTable exposing (..)

import Context exposing (Context)
import Dict
import Element exposing (..)
import Element.Events exposing (onClick)
import Element.Font as Font
import Formatting exposing (..)
import Html.Attributes as HtmlAttr
import Icons exposing (..)
import Iso8601
import Network exposing (Host, Model, Msg(..), NetworkNode, SortBy(..), SortMode(..), viewSummaryWidgets)
import Palette exposing (Palette, darkish)
import Round
import Svg.Attributes exposing (visibility)
import Tooltip exposing (..)
import Widgets exposing (content, remoteDataView)


nodesPerPage : Int
nodesPerPage =
    50


visible : Bool -> Attribute msg
visible b =
    htmlAttribute <|
        HtmlAttr.style "visibility" <|
            if b then
                "visible"

            else
                "hidden"


view : Context a -> Model -> Element Msg
view ctx model =
    content <|
        column [ spacing 30, width fill ]
            [ viewSummaryWidgets ctx model.nodes
            , remoteDataView ctx.palette
                (\nodes ->
                    let
                        nodesFrom =
                            model.nodePage * nodesPerPage

                        listNodes =
                            nodes
                                |> Dict.toList
                                |> List.map Tuple.second
                                |> sortNodesMode model.sortMode

                        visibleNodes =
                            listNodes
                                |> List.drop nodesFrom
                                |> List.take nodesPerPage

                        nodesTo =
                            nodesFrom + List.length visibleNodes

                        totalPages =
                            ceiling (toFloat (List.length listNodes) / toFloat nodesPerPage) - 1
                    in
                    column [ spacing 10, width fill ]
                        [ nodesTable ctx model.sortMode visibleNodes
                        , row [ centerX ]
                            [ el
                                [ onClick PreviousNodePage
                                , pointer
                                , visible (model.nodePage > 0)
                                ]
                                (text "Prev ")
                            , text <| String.fromInt (nodesFrom + 1) ++ " - " ++ String.fromInt nodesTo
                            , el
                                [ onClick NextNodePage
                                , pointer
                                , visible (model.nodePage < totalPages)
                                ]
                                (text " Next")
                            ]
                        ]
                )
                model.nodes
            ]


nodesTable : Context a -> SortMode -> List NetworkNode -> Element Msg
nodesTable ctx sortMode nodes =
    if List.length nodes == 0 then
        row [ Font.color ctx.palette.fg2 ] [ text "Waiting for node statistics..." ]

    else
        Element.table
            [ spacing 10
            , Font.color ctx.palette.fg2
            , Font.alignRight
            , paddingEach { left = 0, top = 20, right = 0, bottom = 2 }
            ]
            { data = nodes
            , columns =
                [ { header = el [ Font.alignLeft ] <| sortableHeader ctx sortMode SortName "Name"
                  , width = px 250
                  , view =
                        \node ->
                            el
                                [ pointer
                                , onClick (NodeClicked node.nodeId)
                                , Font.alignLeft
                                ]
                                (text <| ellipsis 30 node.nodeName)
                  }
                , { header = sortableHeaderWithTooltip ctx sortMode SortBaker "Baker" "Baker ID when baking"
                  , width = fill
                  , view =
                        \node ->
                            case node.consensusBakerId of
                                Just id ->
                                    text <| String.fromFloat id

                                Nothing ->
                                    el [ Font.color ctx.palette.fg3 ] <| text "n/a"
                  }
                , { header = sortableHeader ctx sortMode SortUptime "Uptime"
                  , width = fill
                  , view =
                        \node ->
                            text <| asTimeAgoDuration node.uptime
                  }
                , { header = sortableHeader ctx sortMode SortClient "Client"
                  , width = fill
                  , view =
                        \node ->
                            text node.client
                  }
                , { header = sortableHeader ctx sortMode SortAvgPing "Avg Ping"
                  , width = fill
                  , view =
                        \node -> formatPing ctx.palette node.averagePing
                  }
                , { header = sortableHeader ctx sortMode SortPeers "Peers"
                  , width = fill
                  , view =
                        \node ->
                            if node.peersCount == 0 then
                                el [ Font.color ctx.palette.failure ] (text "0")

                            else
                                text <| String.fromFloat node.peersCount
                  }
                , { header = sortableHeaderWithTooltip ctx sortMode SortSent "Sent" "Number of messages sent by the node"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.packetsSent
                  }
                , { header = sortableHeaderWithTooltip ctx sortMode SortReceived "Received" "Number of messages received by the node"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.packetsReceived
                  }
                , { header = sortableHeaderWithTooltip ctx sortMode SortBlock "Block" "Best block known to the node"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.left 6 node.bestBlock
                  }
                , { header = sortableHeader ctx sortMode SortHeight "Length"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.bestBlockHeight
                  }
                , { header = sortableHeader ctx sortMode SortFinalizedBlock "Fin Block"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.left 6 node.finalizedBlock
                  }
                , { header = sortableHeader ctx sortMode SortFinalizedHeight "Fin Length"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.finalizedBlockHeight
                  }
                , { header = el [ Font.color ctx.palette.fg1 ] (text "Last Fin")
                  , width = fill
                  , view =
                        \node ->
                            Maybe.withDefault "" node.finalizedTime
                                |> Iso8601.toTime
                                |> Result.toMaybe
                                |> asSecondsAgo ctx.time
                                |> text
                  }
                , { header = el [ Font.color ctx.palette.fg1 ] (text "Block EMA")
                  , width = fill
                  , view =
                        \node ->
                            text <| Round.round 2 <| Maybe.withDefault 0 node.blockArrivePeriodEMA
                  }
                , { header = el [ Font.color ctx.palette.fg1 ] (text "Fin EMA")
                  , width = fill
                  , view =
                        \node ->
                            text <| Round.round 2 <| Maybe.withDefault 0 node.finalizationPeriodEMA
                  }
                ]
            }


sortableHeader : Context a -> SortMode -> SortBy -> String -> Element Msg
sortableHeader ctx sortMode sortBy name =
    let
        withIcon arrow =
            row
                [ spacing 5
                , Font.color ctx.palette.fg1
                , pointer
                , onClick <| SortSet sortBy
                ]
                [ el [ alignRight ] (text name)
                , el [ alignRight, moveDown 3 ] (html arrow)
                ]

        withoutIcon =
            el [ onClick <| SortSet sortBy, Font.color ctx.palette.fg1, pointer ] (text name)
    in
    case sortMode of
        SortAsc sortBy_ ->
            if sortBy_ == sortBy then
                withIcon <| Icons.arrow_up 12

            else
                withoutIcon

        SortDesc sortBy_ ->
            if sortBy_ == sortBy then
                withIcon <| Icons.arrow_down 12

            else
                withoutIcon

        SortNone ->
            withoutIcon


sortableHeaderWithTooltip : Context a -> SortMode -> SortBy -> String -> String -> Element Msg
sortableHeaderWithTooltip ctx sortMode sortBy name tooltip =
    let
        withIcon arrow =
            row
                [ spacing 5
                , Font.color ctx.palette.fg1
                , pointer
                , onClick <| SortSet sortBy
                ]
                [ el [ alignRight, stringTooltipAbove ctx tooltip ] (text name)
                , el [ alignRight, moveDown 3 ] (html arrow)
                ]

        withoutIcon =
            el
                [ onClick <| SortSet sortBy
                , Font.color ctx.palette.fg1
                , pointer
                ]
                (el [ alignRight, stringTooltipAbove ctx tooltip ] (text name))
    in
    case sortMode of
        SortAsc sortBy_ ->
            if sortBy_ == sortBy then
                withIcon <| Icons.arrow_up 12

            else
                withoutIcon

        SortDesc sortBy_ ->
            if sortBy_ == sortBy then
                withIcon <| Icons.arrow_down 12

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


sortNodesBy : SortBy -> List NetworkNode -> List NetworkNode
sortNodesBy sortBy listNodes =
    case sortBy of
        SortName ->
            List.sortBy .nodeName listNodes

        SortBaker ->
            List.sortBy (.consensusBakerId >> Maybe.withDefault (1/0)) listNodes

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
