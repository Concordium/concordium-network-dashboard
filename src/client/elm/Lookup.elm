module Lookup exposing (..)

import Context exposing (Theme)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers exposing (..)


type alias Model =
    { searchTextValue : String
    }


init : Model
init =
    { searchTextValue = ""
    }


type Msg
    = SetSearchTextValue String
    | SearchForTransaction String
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSearchTextValue txt ->
            ( { model | searchTextValue = txt }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Theme a -> Model -> Element Msg
view theme model =
    column
        [ centerX
        , spacing 10
        , width
            (fill
                |> maximum 500
            )
        ]
        [ Input.text
            [ Background.color theme.palette.bg1
            , Input.focusedOnLoad
            , Border.color theme.palette.fg2
            , Border.rounded 8
            , onEnter <| SearchForTransaction model.searchTextValue
            ]
            { onChange = SetSearchTextValue
            , text = model.searchTextValue
            , label = Input.labelAbove [ Font.center, paddingXY 5 10, Font.size 20 ] <| text "Lookup a transaction"
            , placeholder = Just <| Input.placeholder [ Font.color theme.palette.fg2 ] <| text "Insert transaction hash"
            }
        , Input.button
            [ centerX
            , Font.center
            , paddingXY 20 10
            , Border.rounded 5
            , Background.color theme.palette.fg3
            , focused
                [ Background.color theme.palette.bg3
                ]
            ]
            { onPress = Just <| SearchForTransaction model.searchTextValue
            , label = el [ Font.color theme.palette.fg1 ] <| text "Lookup"
            }
        ]
