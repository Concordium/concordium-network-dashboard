port module Main exposing (main)

import Browser
import Color exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Markdown


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
    { host : String
    , state : Maybe String
    , uptime : Float -- Milliseconds @TODO figure out how to convert to Int, issue is in JS everything is Double even Ints
    , client : String
    , averagePing : Float -- Milliseconds @TODO as above figure out Int
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
            [ paragraph [] [ markdown "### Concordium Dashboard POC" ]
            , column [ spacing 10 ]
                [ nodesTable model.nodes ]
            ]
        ]
    , title = "Concordium Dashboard"
    }


nodesTable nodes =
    if Dict.size nodes == 0 then
        text "Loading node statistics..."

    else
        Element.table [ spacing 10 ]
            { data = nodes |> Dict.toList |> List.map Tuple.second
            , columns =
                [ { header = text "Name"
                  , width = fill
                  , view =
                        \node ->
                            text node.host
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
                            text <| String.fromFloat node.averagePing
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
        [ column [] [ text node.host ]
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
            ( { model | nodes = Dict.insert node.host node model.nodes }, Cmd.none )


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
    layout [ width fill, padding 10, bgDarkGrey, Font.color grey, Font.size 14 ] <|
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
