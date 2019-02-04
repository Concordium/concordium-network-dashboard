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


port reply : (String -> msg) -> Sub msg


type alias Flags =
    ()


type alias Model =
    { nodes : List Node
    }


type alias Node =
    { name : String
    , state : Maybe String
    }


type Msg
    = Default String
    | ReplyReceived String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { nodes =
            [ { name = "node1", state = Nothing }
            , { name = "node2", state = Just "{somestate}" }
            ]
      }
    , hello "Hello from Elm!"
    )


view : Model -> Browser.Document Msg
view model =
    { body =
        [ theme
            [ paragraph [] [ markdown "### Concordium Dashboard POC" ]
            , column [ spacing 10 ]
                (List.map viewNode model.nodes)
            ]
        ]
    , title = "Concordium Dashboard"
    }


viewNode : Node -> Element msg
viewNode node =
    row []
        [ row [ width (px 100) ] [ text node.name ]
        , row [ width (px 100) ] [ text <| Maybe.withDefault "<No state loaded>" node.state ]
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case Debug.log "Main.update" msg of
        Default string ->
            ( model, Cmd.none )

        ReplyReceived message ->
            let
                _ =
                    Debug.log "ReplyReceived" message
            in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    reply ReplyReceived


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
