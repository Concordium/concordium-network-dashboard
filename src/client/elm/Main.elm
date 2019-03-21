port module Main exposing (main)

import Browser
import Chart
import Colors exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Markdown
import String


port hello : String -> Cmd msg


port nodeInfo : (Node -> msg) -> Sub msg


type alias Flags =
    ()


type alias Host =
    String


type alias Model =
    { nodes : Dict Host Node
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { nodes = Dict.empty }, hello "Hello from Elm!" )


view : Model -> Browser.Document Msg
view model =
    { body =
        [ theme
            [ column [ spacing 20 ]
                [ image [ height (px 20) ] { src = "/assets/images/concordium-logo.png", description = "Concordium Logo" }
                , wrappedRow [ spacing 20 ]
                    [ widgetNumberChart blue_ "Block Height" "/assets/images/icon-blocks-blue.png" (Dict.size model.nodes)
                    , widgetNumber purple "Active Nodes" "/assets/images/icon-nodes-purple.png" (Dict.size model.nodes)
                    , widgetNumber lightBlue "Last Block" "/assets/images/icon-lastblock-lightblue.png" (Dict.size model.nodes)
                    , widgetNumber pink "Avg Block Time" "/assets/images/icon-rocket-pink.png" (Dict.size model.nodes)
                    , widgetNumber green "Block finalized height" "/assets/images/icon-blocksfinal-green.png" (Dict.size model.nodes)
                    , widgetNumber lightBlue "Last finalized block" "/assets/images/icon-blocklastfinal-lightblue.png" (Dict.size model.nodes)
                    , chartTimeseries blue_ "Active Nodes" "/assets/images/icon-blocks-blue.png" (Dict.size model.nodes)
                    ]
                , nodesTable model.nodes
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


nodesTable nodes =
    if Dict.size nodes == 0 then
        row [ Font.color green ] [ text "Waiting for node statistics..." ]

    else
        Element.table [ spacing 10, Font.color green ]
            { data = nodes |> Dict.toList |> List.map Tuple.second
            , columns =
                [ { header = text "Name"
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
                , { header = text "Uptime"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.uptime
                  }
                , { header = text "Client"
                  , width = fill
                  , view =
                        \node ->
                            text node.client
                  }
                , { header = text "Avg Ping"
                  , width = fill
                  , view =
                        \node ->
                            case node.averagePing of
                                Just ping ->
                                    text <| String.fromFloat ping

                                Nothing ->
                                    text "n/a"
                  }
                , { header = text "Peers"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.peersCount
                  }
                , { header = text "Sent"
                  , width = fill
                  , view =
                        \node ->
                            text <| String.fromFloat node.packetsSent
                  }
                , { header = text "Received"
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


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case Debug.log "Main.update" msg of
        Default string ->
            ( model, Cmd.none )

        NodeInfoReceived node ->
            let
                _ =
                    Debug.log "NodeInfoReceived" node
            in
            ( { model | nodes = Dict.insert node.nodeName node model.nodes }, Cmd.none )


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
