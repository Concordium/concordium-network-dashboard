port module Dashboard exposing (Flags, bgDarkGrey, bgWhite, chartTimeseries, hashSnippet, hello, init, main, markdown, nodeInfo, nodeSummariesDecoder, nodesTable, scrollPageToTop, sortNodesBy, sortNodesMode, sortableHeader, subscriptions, theme, update, view, viewNode, widgetNumber, widgetNumberChart, widgetSeconds, widgetText, widgetsForWebsite, worldMap)

import Browser exposing (..)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Chart
import Colors exposing (..)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import List.Extra as List
import Markdown
import Pages.Graph
import Round
import String
import Task
import Time
import Time.Distance exposing (inWordsWithConfig)
import Time.Distance.I18n as I18n
import Time.Extra
import Types exposing (..)
import Url exposing (Url)
import Widgets exposing (..)


port hello : String -> Cmd msg


port nodeInfo : (NetworkNode -> msg) -> Sub msg


nodeSummariesDecoder =
    D.list
        (D.succeed NetworkNode
            |> required "nodeName" D.string
            |> required "nodeId" D.string
            |> required "uptime" D.float
            |> required "client" D.string
            |> required "averagePing" (D.nullable D.float)
            |> required "peersCount" D.float
            |> required "peersList" (D.list D.string)
            |> required "bestBlock" D.string
            |> required "bestBlockHeight" D.float
            |> required "bestArrivedTime" (D.nullable D.string)
            |> required "blockArrivePeriodEMA" (D.nullable D.float)
            |> required "blockArrivePeriodEMSD" (D.nullable D.float)
            |> required "finalizedBlock" D.string
            |> required "finalizedBlockHeight" D.float
            |> required "finalizedTime" (D.nullable D.string)
            |> required "finalizationPeriodEMA" (D.nullable D.float)
            |> required "finalizationPeriodEMSD" (D.nullable D.float)
            |> required "packetsSent" D.float
            |> required "packetsReceived" D.float
        )


type alias Flags =
    { width : Int, height : Int }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { nodes = Dict.empty
      , currentTime = Time.millisToPosix 0
      , key = key
      , currentPage = pathToPage url
      , sortMode = SortNone
      , window = flags
      , selectedNode = Nothing
      , graph = { width = 800, height = 800 }
      }
    , hello "Hello from Elm!"
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Concordium Dashboard"
    , body =
        [ theme <|
            case model.currentPage of
                Dashboard ->
                    [ column [ spacing 20, width fill ]
                        [ header
                        , wrappedRow [ spacing 20, width fill ]
                            [ widgetNumber purple "Active Nodes" "/assets/images/icon-nodes-purple.png" (toFloat <| Dict.size model.nodes)
                            , widgetSeconds blue "Last Block" "/assets/images/icon-lastblock-lightblue.png" (majorityStatFor (\n -> asSecondsAgo model.currentTime (Maybe.withDefault "" n.bestArrivedTime)) -1 model.nodes)
                            , widgetSeconds green "Last finalized block" "/assets/images/icon-blocklastfinal-green.png" (majorityStatFor (\n -> asSecondsAgo model.currentTime (Maybe.withDefault "" n.finalizedTime)) -1 model.nodes)
                            , widgetNumber blue "Block Height" "/assets/images/icon-blocks-blue.png" (majorityStatFor .bestBlockHeight -1 model.nodes)
                            , widgetNumber green "Finalized height" "/assets/images/icon-blocksfinal-green.png" (majorityStatFor .finalizedBlockHeight -1 model.nodes)
                            , widgetText pink "Last Block EMA" "/assets/images/icon-rocket-pink.png" <|
                                averageStatSecondsFor .blockArrivePeriodEMA model.nodes
                            , widgetText pink "Last Finalization EMA" "/assets/images/icon-rocket-pink.png" <|
                                averageStatSecondsFor .finalizationPeriodEMA model.nodes

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

                NodeGraph ->
                    Pages.Graph.view model
        ]
    }


widgetsForWebsite model =
    [ widgetNumber purple "Active Nodes" "/assets/images/icon-nodes-purple.png" (toFloat <| Dict.size model.nodes)
    , widgetSeconds lightBlue "Last Block" "/assets/images/icon-lastblock-lightblue.png" (majorityStatFor (\n -> asSecondsAgo model.currentTime (Maybe.withDefault "" n.bestArrivedTime)) -1 model.nodes)

    -- , widgetSeconds green "Last finalized block" "/assets/images/icon-blocklastfinal-green.png" (majorityStatFor (\n -> asSecondsAgo model.currentTime (Maybe.withDefault "" n.finalizedTime)) -1 model.nodes)
    , widgetNumber blue "Block Height" "/assets/images/icon-blocks-blue.png" (majorityStatFor .bestBlockHeight -1 model.nodes)

    -- , widgetNumber green "Finalized height" "/assets/images/icon-blocksfinal-green.png" (majorityStatFor .finalizedBlockHeight -1 model.nodes)
    , widgetText pink "Last Block EMA" "/assets/images/icon-rocket-pink.png" <|
        averageStatSecondsFor .blockArrivePeriodEMA model.nodes

    -- , widgetText pink "Avg Finalization Time" "/assets/images/icon-rocket-pink.png" <|
    --     averageStatSecondsFor .finalizationPeriodEMA model.nodes
    ]


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


widgetSeconds color title icon value =
    row [ height (px 140), width (fillPortion 1), Background.color moduleGrey, padding 20, spacing 30, Border.rounded 5 ]
        [ column []
            [ row [ Background.color darkGrey, Border.rounded 100, height (px 70), width (px 70) ] [ image [ height (px 35), centerY, centerX ] { src = icon, description = "Decorative icon" } ] ]
        , column [ spacing 20 ]
            [ row [ Font.color color ] [ text <| String.toUpper title ]
            , row [ Font.color color, Font.size 30 ]
                [ text <|
                    if value >= 0 then
                        String.fromInt value ++ "s ago"

                    else
                        "-"
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
                        String.fromFloat value

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
                        String.fromFloat value

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
            , row [ Font.color color, Font.size 30 ] [ text <| String.fromFloat value ]
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
                            el [ pointer, onClick (NodeClicked node.nodeId) ] <| text node.nodeName
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
                ]
            }


hashSnippet hash =
    String.left 6 hash ++ "..."



-- ++ String.right 6 hash


viewNode : NetworkNode -> Element msg
viewNode node =
    row []
        [ column [] [ text node.nodeName ]

        -- , column [] [ text <| Maybe.withDefault "<No state loaded>" node.state ]
        -- , column [] [ text <| Maybe.withDefault "<No state loaded>" node.state ]
        ]


sortableHeader model sortBy name =
    let
        withIcon url =
            row [ spacing 5, Font.color lightGrey ]
                [ el [ onClick <| SortSet sortBy ] (text name)
                , image [ width (px 10) ] { src = url, description = "Sort Ascending Icon" }
                ]

        withoutIcon =
            el [ onClick <| SortSet sortBy, Font.color lightGrey ] (text name)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)

                        -- , onPageExit model.currentPage model
                        -- , onPageInit (pathToPage url) model
                        ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( { model | currentPage = pathToPage url }, scrollPageToTop )

        WindowResized width height ->
            ( { model | window = { width = width, height = height } }, Cmd.none )

        NodeInfoReceived node ->
            ( { model | nodes = Dict.insert node.nodeName node model.nodes }, Cmd.none )

        FetchNodeSummaries _ ->
            ( model, Http.get { url = "/data/nodesSummary", expect = Http.expectJson FetchedNodeSummaries nodeSummariesDecoder } )

        FetchedNodeSummaries r ->
            case r of
                Ok nodeSummaries ->
                    let
                        nodes =
                            nodeSummaries |> List.map (\node -> ( node.nodeName, node )) |> Dict.fromList
                    in
                    ( { model
                        | nodes = nodes

                        -- , selectedNode = nodes |> Dict.toList |> List.head |> Maybe.map Tuple.second
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )

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

        NodeClicked nodeId ->
            let
                selectedNode =
                    case Dict.find (\_ n -> n.nodeId == nodeId) model.nodes of
                        Just ( _, node ) ->
                            Just node

                        Nothing ->
                            Nothing
            in
            ( { model | selectedNode = selectedNode }, Cmd.batch [ Nav.pushUrl model.key (pageToPath NodeGraph), scrollPageToTop ] )

        GraphZoom zoom ->
            ( { model | graph = { width = model.graph.width + zoom, height = model.graph.height + zoom } }, Cmd.none )

        DevResetCache ->
            ( model, Http.get { url = "/dev/reset", expect = Http.expectWhatever NoopHttp } )

        NoopHttp r ->
            ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


scrollPageToTop =
    Task.perform (\_ -> Noop) (Browser.Dom.setViewport 0 0)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ nodeInfo NodeInfoReceived
        , Browser.Events.onResize WindowResized
        , Time.every 1000 CurrentTime
        , Time.every 1000 FetchNodeSummaries
        ]


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