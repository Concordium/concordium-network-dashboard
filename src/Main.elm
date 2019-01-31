port module Main exposing (main)

import Browser
import Color exposing (..)
import Concordium_p2p_rpc
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


port reply : (Int -> msg) -> Sub msg


type alias Flags =
    ()


type alias Model =
    { derp : Int
    }


type Msg
    = Default String
    | ReplyReceived Int


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { derp = 1 }, hello "World" )


view : Model -> Browser.Document Msg
view model =
    { body =
        [ theme
            [ paragraph [] [ markdown "### Concordium Dashboard POC" ]
            , row [ spacing 10 ]
                [ paragraph [] [ markdown "cool" ]
                ]
            ]
        ]
    , title = "Concordium Dashboard"
    }


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
