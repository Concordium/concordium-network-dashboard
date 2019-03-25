port module Main exposing (main)

import Browser
import Chart
import Colors exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Markdown
import Round
import String
import Time exposing (millisToPosix)
import Time.Distance exposing (inWordsWithConfig)
import Time.Distance.I18n as I18n


port hello : String -> Cmd msg


port nodeInfo : (Node -> msg) -> Sub msg


type alias Flags =
    ()


type alias Host =
    String


type alias Model =
    { nodes : Dict Host Node
    , sortMode : SortMode
    }


type alias Node =
    { nodeName : String
    , state : Maybe String
    , uptime : Float -- Milliseconds @TODO figure out how to convert to Int, issue is in JS everything is Double even Ints
    , client : String
    , averagePing : Maybe Float -- Milliseconds @TODO as above figure out Int. Maybe for when 0 nodes
    , peersCount : Float -- @TODO as above figure out Int
    , bestBlockHash : String
    , packetsSent : Float -- @TODO as above figure out Int
    , packetsReceived : Float -- @TODO as above figure out Int
    }


type Msg
    = Default String
    | NodeInfoReceived Node
    | SortSet SortBy


type SortMode
    = SortAsc SortBy
    | SortDesc SortBy
    | SortNone


type SortBy
    = SortName
    | SortUptime
    | SortAvgPing
    | SortPeers
    | SortSent
    | SortReceived


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { nodes = Dict.empty, sortMode = SortNone }, hello "Hello from Elm!" )


view : Model -> Browser.Document Msg
view model =
    { body =
        [ theme
            [ column [ spacing 20 ]
                [ image [ height (px 20) ] { src = "/assets/images/concordium-logo.png", description = "Concordium Logo" }
                , wrappedRow [ spacing 20 ]
                    [ widgetNumber purple "Active Nodes" "/assets/images/icon-nodes-purple.png" (Dict.size model.nodes)
                    , widgetNumberChart blue "Block Height" "/assets/images/icon-blocks-blue.png" (Dict.size model.nodes)
                    , widgetNumber lightBlue "Last Block" "/assets/images/icon-lastblock-lightblue.png" (Dict.size model.nodes)
                    , widgetNumber pink "Avg Block Time" "/assets/images/icon-rocket-pink.png" (Dict.size model.nodes)
                    , widgetNumber green "Block finalized height" "/assets/images/icon-blocksfinal-green.png" (Dict.size model.nodes)
                    , widgetNumber green "Last finalized block" "/assets/images/icon-blocklastfinal-green.png" (Dict.size model.nodes)
                    , worldMap

                    -- , chartTimeseries blue "Active Nodes" "/assets/images/icon-blocks-blue.png" (Dict.size model.nodes)
                    ]
                , nodesTable model (sortNodesMode model.sortMode model.nodes)
                ]
            ]
        ]
    , title = "Concordium Dashboard"
    }


widgetNumber color title icon value =
    row [ height (px 140), width (fillPortion 1), Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
        [ column []
            [ row [ Background.color darkGrey, Border.rounded 100, height (px 70), width (px 70) ] [ image [ height (px 35), centerY, centerX ] { src = icon, description = "Decorative icon" } ] ]
        , column [ spacing 20 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ] [ text <| Debug.toString value ]
            ]
        ]


widgetNumberChart color title icon value =
    row [ Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
        [ column []
            [ row [ Background.color darkGrey, Border.rounded 100, height (px 70), width (px 70) ] [ image [ height (px 40), centerY, centerX ] { src = icon, description = "Decorative icon" } ] ]
        , column [ spacing 20 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ] [ text <| Debug.toString value ]
            ]
        , column [ width (px 200) ]
            [ html Chart.test ]
        ]


chartTimeseries color title icon value =
    row [ Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
        -- @TODO play with this later to finish the chart effect
        -- Background.gradient { angle = pi, steps = [ rgba255 49 178 239 1, rgba255 0 0 0 0 ] }
        [ column [ spacing 20 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ] [ text <| Debug.toString value ]
            ]
        , column [ width (px 200) ]
            [ html Chart.test ]
        ]


worldMap =
    row [ height (px 260), width (fillPortion 1), Background.color moduleGrey, Border.rounded 5 ]
        [ column [ spacing 0 ]
            [ row [ Font.color blue, paddingXY 20 0 ] [ text <| String.toUpper "Node Locations" ]
            , image [ height (px 220), centerY, centerX ] { src = "/assets/images/world.svg", description = "World Map" }
            ]
        ]


nodesTable model nodes =
    if List.length nodes == 0 then
        row [ Font.color green ] [ text "Waiting for node statistics..." ]

    else
        Element.table [ spacing 10, Font.color green ]
            { data = nodes
            , columns =
                [ { header = sortableHeader model SortName "Name"
                  , width = fill
                  , view =
                        \node ->
                            text node.nodeName
                  }
                , { header = text "State"
                  , width = fill
                  , view =
                        \node ->
                            text <| Maybe.withDefault "<No state loaded>" node.state
                  }
                , { header = sortableHeader model SortUptime "Uptime"
                  , width = fill
                  , view =
                        \node ->
                            let
                                point =
                                    1552573958904

                                timePoint =
                                    millisToPosix point

                                offsetTimePoint =
                                    millisToPosix (point - round node.uptime)

                                words =
                                    inWordsWithConfig { withAffix = False } I18n.en offsetTimePoint timePoint
                            in
                            text words
                  }
                , { header = text "Client"
                  , width = fill
                  , view =
                        \node ->
                            text node.client
                  }
                , { header = sortableHeader model SortAvgPing "Avg Ping"
                  , width = fill
                  , view =
                        \node ->
                            case node.averagePing of
                                Just ping ->
                                    if ping < 1000 then
                                        text <| Round.round 2 ping ++ "ms"

                                    else if ping < 1000 * 60 then
                                        el [ Font.color orange ] (text <| Round.round 2 (ping / 1000) ++ "s")

                                    else
                                        el [ Font.color red ] (text <| "> 60s")

                                Nothing ->
                                    el [ Font.color red ] (text "n/a")
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
                , { header = text "Last Block"
                  , width = fill
                  , view =
                        \node ->
                            text node.bestBlockHash
                  }
                ]
            }


viewNode : Node -> Element msg
viewNode node =
    row []
        [ column [] [ text node.nodeName ]
        , column [] [ text <| Maybe.withDefault "<No state loaded>" node.state ]
        , column [] [ text <| Maybe.withDefault "<No state loaded>" node.state ]
        ]


sortableHeader model sortBy name =
    let
        withIcon url =
            row [ spacing 5 ]
                [ el [ onClick <| SortSet sortBy ] (text name)
                , image [ width (px 10) ] { src = url, description = "Sort Ascending Icon" }
                ]

        withoutIcon =
            el [ onClick <| SortSet sortBy ] (text name)
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


sortNodesMode : SortMode -> Dict String Node -> List Node
sortNodesMode sortMode nodes =
    let
        listNodes =
            nodes |> Dict.toList |> List.map Tuple.second
    in
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


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Default string ->
            ( model, Cmd.none )

        NodeInfoReceived node ->
            ( { model | nodes = Dict.insert node.nodeName node model.nodes }, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions model =
    nodeInfo NodeInfoReceived


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


theme : List (Element msg) -> Html.Html msg
theme x =
    layout
        [ width fill
        , padding 20
        , bgDarkGrey
        , Font.color grey
        , Font.family [ Font.typeface "Exo" ]
        , Font.size 14
        ]
    <|
        column []
            x


markdown : String -> Element msg
markdown string =
    Element.html <| Markdown.toHtml [] string


bgDarkGrey : Attr decorative msg
bgDarkGrey =
    Background.color <| darkGrey


bgWhite : Attr decorative msg
bgWhite =
    Background.color <| white
