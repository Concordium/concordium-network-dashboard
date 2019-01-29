module Main exposing (main)

import Browser
import Color exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Markdown


type alias Model =
    { derp : Int
    }


type Msg
    = Default String


init : ( Model, Cmd msg )
init =
    ( { derp = 1
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    theme
        [ paragraph [] [ markdown "### Concordium Dashboard POC" ]
        , row [ spacing 10 ]
            [ paragraph [] [ markdown "cool" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case Debug.log "Main.update" msg of
        Default string ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


theme : List (Element msg) -> Html.Html msg
theme x =
    layout [ width fill, padding 10, bgGrey ] <|
        column []
            [ column [ bgWhite, padding 20, spacing 20 ] x
            ]


markdown : String -> Element msg
markdown string =
    Element.html <| Markdown.toHtml [] string


bgGrey : Attr decorative msg
bgGrey =
    Background.color <| grey


bgWhite : Attr decorative msg
bgWhite =
    Background.color <| white
