module Dashboard.Widgets exposing (..)

import Context exposing (Context)
import Dashboard.Formatting exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Helpers exposing (..)
import Loading
import Palette exposing (Palette, toHex, veryDark, withAlphaEl)
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Trend.Math
import Types exposing (..)


type alias Widget =
    { color : Color
    , title : String
    , description : String
    , icon : String
    , value : WebData String
    , subvalue : Maybe String
    }


content : Element msg -> Element msg
content e =
    el [ width fill, height fill, paddingXY 30 0 ] e


viewSummaryWidgets : Context a -> WebData (Dict Host NetworkNode) -> Element msg
viewSummaryWidgets ctx remoteNodes =
    column [ spacing 12, width fill ]
        [ wrappedRow [ spacing 12, width fill ]
            (List.map (viewWidget ctx)
                [ { color = ctx.palette.c3
                  , title = "Active nodes"
                  , description = ""
                  , icon = "/assets/images/icon-nodes-purple.png"
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
                  , icon = "/assets/images/icon-lastblock-lightblue.png"
                  , value =
                        RemoteData.map
                            (\nodes ->
                                majorityStatFor
                                    (\n ->
                                        asSecondsAgo
                                            ctx.time
                                            (Maybe.withDefault "" n.bestArrivedTime)
                                    )
                                    ""
                                    nodes
                            )
                            remoteNodes
                  , subvalue = Nothing
                  }
                , { color = ctx.palette.c2
                  , title = "Last Finalization"
                  , description = ""
                  , icon = "/assets/images/icon-blocklastfinal-green.png"
                  , value =
                        RemoteData.map
                            (\nodes ->
                                asSecondsAgo ctx.time
                                    (Maybe.withDefault ""
                                        (withinHighestStatFor
                                            .finalizedBlockHeight
                                            ""
                                            nodes
                                            .finalizedTime
                                        )
                                    )
                            )
                            remoteNodes
                  , subvalue = Nothing
                  }
                , { color = ctx.palette.c1
                  , title = "Chain Length"
                  , description = ""
                  , icon = "/assets/images/icon-blocks-blue.png"
                  , value =
                        RemoteData.map
                            (\nodes -> String.fromInt <| round (majorityStatFor .bestBlockHeight -1 nodes))
                            remoteNodes
                  , subvalue = Nothing
                  }
                , { color = ctx.palette.c2
                  , title = "Fin Length"
                  , description = ""
                  , icon = "/assets/images/icon-blocksfinal-green.png"
                  , value =
                        RemoteData.map
                            (\nodes -> String.fromInt <| round (majorityStatFor .finalizedBlockHeight -1 nodes))
                            remoteNodes
                  , subvalue = Nothing
                  }
                ]
            )
        , wrappedRow [ spacing 12, width fill ]
            (List.map (viewWidget ctx)
                [ { color = ctx.palette.c4
                  , title = "Block Time"
                  , description = "The median of nodes' exponential moving average of the interval between verified blocks"
                  , icon = "/assets/images/icon-rocket-pink.png"
                  , value =
                        RemoteData.map
                            (\nodes -> averageStatSecondsFor .blockArrivePeriodEMA nodes)
                            remoteNodes
                  , subvalue =
                        Nothing

                  -- RemoteData.map
                  --     (\nodes ->
                  --         nodes
                  --             |> Dict.toList
                  --             |> List.map Tuple.second
                  --             |> List.map .blockArrivePeriodEMA
                  --             |> justs
                  --             |> Trend.Math.stddev
                  --             |> Result.map (Round.round 2)
                  --             |> Result.withDefault "-"
                  --             |> (++) "stddev: "
                  --     )
                  --     remoteNodes
                  --     |> RemoteData.toMaybe
                  }
                , { color = ctx.palette.c4
                  , title = "Finalization Time"
                  , description = "The median of nodes' exponential moving average of the interval between finalizations"
                  , icon = "/assets/images/icon-rocket-pink.png"
                  , value =
                        RemoteData.map
                            (\nodes -> averageStatSecondsFor .finalizationPeriodEMA nodes)
                            remoteNodes
                  , subvalue =
                        Nothing

                  -- RemoteData.map
                  --     (\nodes ->
                  --         nodes
                  --             |> Dict.toList
                  --             |> List.map Tuple.second
                  --             |> List.map .blockArrivePeriodEMA
                  --             |> justs
                  --             |> Trend.Math.stddev
                  --             |> Result.map (Round.round 2)
                  --             |> Result.withDefault "-"
                  --             |> (++) "stddev: "
                  --     )
                  --     remoteNodes
                  --     |> RemoteData.toMaybe
                  }

                -- , worldMap
                -- , chartTimeseries blue "Active Nodes" "/assets/images/icon-blocks-blue.png" (Dict.size nodes)
                ]
            )
        ]


widgetsForWebsite : Context a -> WebData (Dict Host NetworkNode) -> List (Element msg)
widgetsForWebsite ctx remoteNodes =
    -- active nodes, last block, block height, last block ema
    List.map (viewWidget ctx)
        [ { color = ctx.palette.c3
          , title = "Active nodes"
          , description = ""
          , icon = "/assets/images/icon-nodes-purple.png"
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
          , icon = "/assets/images/icon-lastblock-lightblue.png"
          , value =
                RemoteData.map
                    (\nodes ->
                        majorityStatFor
                            (\n ->
                                asSecondsAgo
                                    ctx.time
                                    (Maybe.withDefault "" n.bestArrivedTime)
                            )
                            ""
                            nodes
                    )
                    remoteNodes
          , subvalue = Nothing
          }
        , { color = ctx.palette.c1
          , title = "Chain Length"
          , description = ""
          , icon = "/assets/images/icon-blocks-blue.png"
          , value =
                RemoteData.map
                    (\nodes -> String.fromInt <| round (majorityStatFor .bestBlockHeight -1 nodes))
                    remoteNodes
          , subvalue = Nothing
          }
        , { color = ctx.palette.c4
          , title = "Last Block EMA"
          , description = ""
          , icon = "/assets/images/icon-rocket-pink.png"
          , value =
                RemoteData.map
                    (\nodes -> averageStatSecondsFor .blockArrivePeriodEMA nodes)
                    remoteNodes
          , subvalue =
                RemoteData.map
                    (\nodes ->
                        nodes
                            |> Dict.toList
                            |> List.map Tuple.second
                            |> List.map .blockArrivePeriodEMA
                            |> justs
                            |> Trend.Math.stddev
                            |> Result.map (Round.round 2)
                            |> Result.withDefault "-"
                            |> (++) "stddev: "
                    )
                    remoteNodes
                    |> RemoteData.toMaybe
          }
        ]


viewTile : Palette Color -> Element msg -> Element msg
viewTile palette tileContent =
    el
        [ height (px 90)
        , width (fillPortion 1)
        , padding 10
        , Background.color palette.bg2
        , Border.rounded 6
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 15
            , color = rgba 0 0 0 0.1
            }
        , Border.color (Palette.lightish palette.bg2)
        , Border.width 1
        ]
        tileContent


viewWidget : Context a -> Widget -> Element msg
viewWidget ctx widget =
    viewTile ctx.palette
        (row [ spacing 10, centerY ]
            [ el
                [ Background.color ctx.palette.bg1
                , Border.rounded 100
                , height (px 50)
                , width (px 50)
                ]
                (image [ height (px 25), centerY, centerX ]
                    { src = widget.icon, description = "Decorative Icon" }
                )
            , column [ spacing 10 ]
                [ row []
                    [ el [ Font.color (withAlphaEl 0.7 widget.color) ]
                        (text <| String.toUpper widget.title ++ " ")
                    , if widget.description == "" then
                        none

                      else
                        el
                            [ Background.color ctx.palette.bg3
                            , Font.color ctx.palette.fg3
                            , Font.size 10
                            , Border.rounded 10
                            , paddingXY 10 5
                            , stringTooltipAboveWidget ctx (paragraph [ Font.size 14, width (fill |> minimum 300) ] [ text widget.description ])
                            ]
                            (text "i")
                    ]
                , column [ Font.color widget.color, Font.size 25 ]
                    [ remoteDataView ctx.palette text widget.value ]
                , widget.subvalue
                    |> Maybe.map
                        (\subvalue ->
                            el
                                [ Font.color (withAlphaEl 0.5 widget.color) ]
                                (text subvalue)
                        )
                    |> Maybe.withDefault none
                ]
            ]
        )



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


worldMap : Palette Color -> Element msg
worldMap palette =
    row [ height (px 260), width (fillPortion 1), Background.color palette.bg2, Border.rounded 5 ]
        [ column [ spacing 0 ]
            [ row [ Font.color palette.c2, paddingXY 20 0 ] [ text <| String.toUpper "Node Locations" ]
            , image [ height (px 220), centerY, centerX ] { src = "/assets/images/world.svg", description = "World Map" }
            ]
        ]


nodePeersOnly : Dict Host NetworkNode -> Dict Host NetworkNode
nodePeersOnly nodes =
    -- @TODO remove "" case when new collector is deployed
    nodes |> Dict.filter (\k n -> n.peerType == "Node" || n.peerType == "")



-- Remote Data Helper


remoteDataView : Palette Color -> (c -> Element msg) -> RemoteData e c -> Element msg
remoteDataView palette successView remoteData =
    case remoteData of
        NotAsked ->
            none

        Loading ->
            loader palette.fg3

        Failure error ->
            badge palette.warning "Error"

        Success data ->
            successView data


loader : Element.Color -> Element msg
loader color =
    let
        defaultConfig =
            Loading.defaultConfig
    in
    el [ padding 4, centerX, centerY ]
        (html <|
            Loading.render
                Loading.DoubleBounce
                { defaultConfig | color = toHex color, size = 22 }
                Loading.On
        )


badge : Color -> String -> Element msg
badge color label =
    el
        [ padding 7
        , Background.color color
        , Border.rounded 1000
        , Font.size 14
        , Font.color <| veryDark color
        , Font.bold
        ]
        (el [ centerX, centerY, moveUp 1 ] (text label))
