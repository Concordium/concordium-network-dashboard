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


port nodeInfo : ({ host : String, state : Maybe String, uptime : Float } -> msg) -> Sub msg


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
    , uptime : Float
    }


type Msg
    = Default String
    | NodeInfoReceived { host : String, state : Maybe String, uptime : Float }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { nodes =
            [ { host = "node1", state = Nothing, uptime = 0 }
            , { host = "node2", state = Just "{somestate}", uptime = 0 }
            ]
                |> List.map (\node -> ( node.host, node ))
                |> Dict.fromList
      }
    , hello "Hello from Elm!"
    )


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
    layout [ width fill, padding 10, bgDarkGrey, Font.color grey ] <|
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
