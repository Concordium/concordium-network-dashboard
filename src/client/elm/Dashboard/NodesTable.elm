module Dashboard.NodesTable exposing (..)

import Context exposing (Context)
import Dashboard.Formatting exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Events exposing (onClick)
import Element.Font as Font
import Palette exposing (Palette)
import Round
import Types exposing (..)


nodesTable : Context a -> SortMode -> List NetworkNode -> Element Msg
nodesTable ctx sortMode nodes =
    if List.length nodes == 0 then
        row [ Font.color ctx.palette.fg2 ] [ text "Waiting for node statistics..." ]

    else
        Element.table [ spacing 10, Font.color ctx.palette.fg2, scrollbarX, Font.alignRight, paddingXY 0 2 ]
            { data = nodes
            , columns =
                [ { header = el [ Font.alignLeft ] <| sortableHeader ctx.palette sortMode SortName "Name"
                  , width = fill
                  , view =
                        \node ->
                            el
                                [ pointer
                                , onClick (NodeClicked node.nodeId)
                                , Font.alignLeft
                                ]
                                (text <| ellipsis 30 node.nodeName)
                  }

                --, { header = text "State"
                --  , width = fill
                --, view =
                --      \node ->
                --           text <| Maybe.withDefault "<No state loaded>" node.state
                --}
                , { header = sortableHeader ctx.palette sortMode SortBaker "Baker"
                  , width = fill
                  , view =
                        \node ->
                            case node.consensusBakerId of
                                Just id ->
                                    -- el [ Font.color ctx.palette.fg3 ] <| text "n/a"
                                    text <| String.fromFloat id

                                Nothing ->
                                    el [ Font.color ctx.palette.fg3 ] <| text "n/a"
                  }
                , { header = sortableHeader ctx.palette sortMode SortUptime "Uptime"
                  , width = fill
                  , view =
                        \node ->
                            text <| asTimeAgoDuration node.uptime
                  }
                , { header = sortableHeader ctx.palette sortMode SortClient "Client"
                  , width = fill
                  , view =
                        \node ->
                            text node.client
                  }
                , { header = sortableHeader ctx.palette sortMode SortAvgPing "Avg Ping"
                  , width = fill
                  , view =
                        \node -> formatPing ctx.palette node.averagePing
                  }
                , { header = sortableHeader ctx.palette sortMode SortPeers "Peers"
                  , width = fill
                  , view =
                        \node ->
                            if node.peersCount == 0 then
                                el [ Font.color ctx.palette.failure ] (text "0")

                            else
                                text <| String.fromFloat node.peersCount
                  }
                , { header = sortableHeader ctx.palette sortMode SortSent "Sent"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.packetsSent
                  }
                , { header = sortableHeader ctx.palette sortMode SortReceived "Received"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.packetsReceived
                  }
                , { header = sortableHeader ctx.palette sortMode SortBlock "Block"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.left 6 node.bestBlock
                  }
                , { header = sortableHeader ctx.palette sortMode SortHeight "Length"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.bestBlockHeight
                  }
                , { header = sortableHeader ctx.palette sortMode SortFinalizedBlock "Fin Block"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.left 6 node.finalizedBlock
                  }
                , { header = sortableHeader ctx.palette sortMode SortFinalizedHeight "Fin Length"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.finalizedBlockHeight
                  }
                , { header = el [ Font.color ctx.palette.fg1 ] (text "Last Fin")
                  , width = fill
                  , view =
                        \node ->
                            text <| asSecondsAgo ctx.time (Maybe.withDefault "" node.finalizedTime)
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


hashSnippet hash =
    String.left 6 hash ++ "..."


sortableHeader : Palette Color -> SortMode -> SortBy -> String -> Element Msg
sortableHeader palette sortMode sortBy name =
    let
        withIcon url =
            row [ spacing 5, Font.color palette.fg1, pointer ]
                [ el [ onClick <| SortSet sortBy ] (text name)
                , image [ width (px 10) ] { src = url, description = "Sort Ascending Icon" }
                ]

        withoutIcon =
            el [ onClick <| SortSet sortBy, Font.color palette.fg1, pointer ] (text name)
    in
    case sortMode of
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

        SortBaker ->
            List.sortBy (.consensusBakerId >> Maybe.withDefault 0) listNodes

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
