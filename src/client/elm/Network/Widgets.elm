module Network.Widgets exposing (..)

import Context exposing (Context)
import Network.Formatting exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Helpers exposing (..)
import Loading
import Palette exposing (Palette, toHex, veryDark, withAlphaEl)
import RemoteData exposing (RemoteData(..), WebData)
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
                  , title = "Finalized Length"
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
                  , description = "Average time between verified blocks"
                  , icon = "/assets/images/icon-rocket-pink.png"
                  , value =
                        RemoteData.map
                            (\nodes -> averageStatSecondsFor .blockArrivePeriodEMA nodes)
                            remoteNodes
                  , subvalue =
                        Nothing
                  }
                , { color = ctx.palette.c4
                  , title = "Finalization Time"
                  , description = "Average time between completed finalizations"
                  , icon = "/assets/images/icon-rocket-pink.png"
                  , value =
                        RemoteData.map
                            (\nodes -> averageStatSecondsFor .finalizationPeriodEMA nodes)
                            remoteNodes
                  , subvalue =
                        Nothing
                  }
                ]
            )
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


nodePeersOnly : Dict Host NetworkNode -> Dict Host NetworkNode
nodePeersOnly nodes =
    nodes |> Dict.filter (\_ n -> n.peerType == "Node")



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
