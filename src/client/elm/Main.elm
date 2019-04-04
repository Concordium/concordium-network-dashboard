port module Main exposing (main)

import Browser exposing (..)
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
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
import List.Extra as List
import Markdown
import NetworkGraph
import Round
import String
import Time exposing (millisToPosix)
import Time.Distance exposing (inWordsWithConfig)
import Time.Distance.I18n as I18n
import Url exposing (Url)


port hello : String -> Cmd msg


port nodeInfo : (Node -> msg) -> Sub msg


type alias Flags =
    { width : Int, height : Int }


type alias Host =
    String


type alias Model =
    { window : { width : Int, height : Int }
    , nodes : Dict Host Node
    , sortMode : SortMode
    }


type alias Node =
    { nodeName : String
    , nodeId : String

    --   , state : Maybe String
    , uptime : Float -- Milliseconds @TODO figure out how to convert to Int, issue is in JS everything is Double even Ints
    , client : String
    , averagePing : Maybe Float -- Milliseconds @TODO as above figure out Int. Maybe for when 0 nodes
    , peersCount : Float -- @TODO as above figure out Int
    , peersList : List String
    , bestBlock : String
    , bestBlockHeight : Float
    , finalizedBlock : String
    , finalizedBlockHeight : Float
    , packetsSent : Float -- @TODO as above figure out Int
    , packetsReceived : Float -- @TODO as above figure out Int
    }


type Msg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | WindowResized Int Int
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


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { nodes = Dict.empty, sortMode = SortNone, window = flags }, hello "Hello from Elm!" )


majorityStatFor getter default nodes =
    -- For the given node attribute, finds majority value across all nodes and returns that, or the default if unknown
    let
        stats =
            nodes
                |> Dict.toList
                |> List.map Tuple.second
                |> List.map getter

        highestResult =
            stats
                |> List.foldl
                    (\v dict ->
                        Dict.update v
                            (\mCount ->
                                case mCount of
                                    Just count ->
                                        Just <| count + 1

                                    Nothing ->
                                        Just 1
                            )
                            dict
                    )
                    Dict.empty
                |> Dict.toList
                |> List.maximumBy (\( attr, count ) -> count)
    in
    case highestResult of
        Just ( highestSeenKey, groupedDictCount ) ->
            highestSeenKey

        Nothing ->
            default


view : Model -> Browser.Document Msg
view model =
    { body =
        [ theme
            [ column [ spacing 20, width fill ]
                [ image [ height (px 20) ] { src = "/assets/images/concordium-logo.png", description = "Concordium Logo" }
                , wrappedRow [ spacing 20, width fill ]
                    [ widgetNumber purple "Active Nodes" "/assets/images/icon-nodes-purple.png" (Dict.size model.nodes)
                    , widgetNumberChart blue "Block Height" "/assets/images/icon-blocks-blue.png" (majorityStatFor .bestBlockHeight -1 model.nodes)
                    , widgetText lightBlue "Last Block" "/assets/images/icon-lastblock-lightblue.png" (majorityStatFor (\n -> hashSnippet n.bestBlock) "-" model.nodes)
                    , widgetNumber pink "Avg Block Time" "/assets/images/icon-rocket-pink.png" -1
                    , widgetNumber green "Block finalized height" "/assets/images/icon-blocksfinal-green.png" (majorityStatFor .finalizedBlockHeight -1 model.nodes)
                    , widgetText green "Last finalized block" "/assets/images/icon-blocklastfinal-green.png" (majorityStatFor (\n -> hashSnippet n.finalizedBlock) "-" model.nodes)
                    , column [ height (px 300), width (px 300), Background.color moduleGrey, Border.rounded 5 ] [ html <| NetworkGraph.agedRelations model.nodes ]

                    -- , worldMap
                    -- , chartTimeseries blue "Active Nodes" "/assets/images/icon-blocks-blue.png" (Dict.size model.nodes)
                    ]
                , let
                    listNodes =
                        model.nodes |> Dict.toList |> List.map Tuple.second

                    sortedNodes =
                        sortNodesMode model.sortMode listNodes
                  in
                  if model.window.width < 1800 then
                    nodesTable model sortedNodes

                  else
                    let
                        ( nodes1, nodes2 ) =
                            -- Ceiling so we always end up with longer part of odd-numbered list first
                            List.splitAt (toFloat (List.length listNodes) / 2 |> ceiling) sortedNodes
                    in
                    row [ spacing 20, width fill ]
                        [ nodesTable model (sortNodesMode model.sortMode nodes1)
                        , column [ height fill, width (px 4), Background.color blue ] []
                        , nodesTable model (sortNodesMode model.sortMode nodes2)
                        ]
                ]
            ]
        ]
    , title = "Concordium Dashboard"
    }


widgetText color title icon value =
    row [ height (px 140), width (fillPortion 1), Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
        [ column []
            [ row [ Background.color darkGrey, Border.rounded 100, height (px 70), width (px 70) ] [ image [ height (px 35), centerY, centerX ] { src = icon, description = "Decorative icon" } ] ]
        , column [ spacing 20 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ]
                [ text value
                ]
            ]
        ]


widgetNumber color title icon value =
    row [ height (px 140), width (fillPortion 1), Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
        [ column []
            [ row [ Background.color darkGrey, Border.rounded 100, height (px 70), width (px 70) ] [ image [ height (px 35), centerY, centerX ] { src = icon, description = "Decorative icon" } ] ]
        , column [ spacing 20 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ]
                [ text <|
                    if value >= 0 then
                        Debug.toString value

                    else
                        "-"
                ]
            ]
        ]


widgetNumberChart color title icon value =
    row [ Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
        [ column []
            [ row [ Background.color darkGrey, Border.rounded 100, height (px 70), width (px 70) ] [ image [ height (px 40), centerY, centerX ] { src = icon, description = "Decorative icon" } ] ]
        , column [ spacing 20 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ]
                [ text <|
                    if value >= 0 then
                        Debug.toString value

                    else
                        "-"
                ]
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
        Element.table [ spacing 10, Font.color green, alignTop, width fill ]
            { data = nodes
            , columns =
                [ { header = sortableHeader model SortName "Name"
                  , width = fill
                  , view =
                        \node ->
                            text node.nodeName
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
                , { header = text "Block"
                  , width = fill
                  , view =
                        \node ->
                            text <| hashSnippet node.bestBlock
                  }
                , { header = text "Block Height"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.bestBlockHeight
                  }
                , { header = text "Finalized Block"
                  , width = fill
                  , view =
                        \node ->
                            text <| hashSnippet node.finalizedBlock
                  }
                , { header = text "Finalized Height"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.finalizedBlockHeight
                  }
                ]
            }


hashSnippet hash =
    String.left 6 hash ++ "..."



-- ++ String.right 6 hash


viewNode : Node -> Element msg
viewNode node =
    row []
        [ column [] [ text node.nodeName ]

        -- , column [] [ text <| Maybe.withDefault "<No state loaded>" node.state ]
        -- , column [] [ text <| Maybe.withDefault "<No state loaded>" node.state ]
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


sortNodesMode : SortMode -> List Node -> List Node
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
        UrlClicked urlRequest ->
            ( model, Cmd.none )

        -- case urlRequest of
        --     Internal url ->
        --         ( { model | currentPage = pathToPage url }
        --         , Nav.pushUrl model.key (Url.toString url)
        --         )
        --
        --     External url ->
        --         ( model
        --         , Nav.load url
        --         )
        UrlChanged url ->
            ( model, Cmd.none )

        WindowResized width height ->
            ( { model | window = { width = width, height = height } }, Cmd.none )

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
    Sub.batch [ nodeInfo NodeInfoReceived, Browser.Events.onResize WindowResized ]


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
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
        column [ width fill ]
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
