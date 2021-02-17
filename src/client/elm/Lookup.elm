module Lookup exposing (..)

import Api exposing (ApiResult)
import Browser.Navigation as Nav
import Context exposing (Theme)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Helpers exposing (..)
import RemoteData exposing (WebData)
import Route exposing (Route)
import Types as T
import Widgets


type alias Model =
    { navigationKey : Nav.Key
    , searchTextValue : String
    , transactionStatusResult : WebData Api.TransactionStatus
    }


init : Nav.Key -> Model
init navigationKey =
    { navigationKey = navigationKey
    , searchTextValue = ""
    , transactionStatusResult = RemoteData.NotAsked
    }


type Msg
    = SetSearchTextValue String
    | SearchForTransaction T.TxHash
    | ReceivedTransactionStatus (ApiResult Api.TransactionStatus)
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSearchTextValue txt ->
            ( { model | searchTextValue = txt }, Cmd.none )

        SearchForTransaction txHash ->
            ( { model | transactionStatusResult = RemoteData.Loading }
            , Nav.pushUrl model.navigationKey <| Route.toString <| Route.LookupTransaction txHash
            )

        ReceivedTransactionStatus result ->
            ( { model | transactionStatusResult = RemoteData.fromResult result }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Theme a -> Model -> Element Msg
view theme model =
    column
        [ spacing 10
        , width fill
        ]
        [ viewTransactionSearch theme model
        , viewTransactionStatusWebData theme model.transactionStatusResult
        ]


viewTransactionSearch : Theme a -> Model -> Element Msg
viewTransactionSearch theme model =
    column
        [ centerX
        , spacing 10
        , width
            (fill
                |> maximum 580
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
            , placeholder = Just <| Input.placeholder [ Font.color theme.palette.fg2 ] <| text "Transaction hash"
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


viewTransactionStatusWebData : Theme a -> WebData Api.TransactionStatus -> Element Msg
viewTransactionStatusWebData theme remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            Element.none

        RemoteData.Loading ->
            Widgets.loader theme.palette.fg3

        RemoteData.Failure error ->
            el [ Font.color theme.palette.danger ] (paragraph [] [ text <| "Error: " ++ Widgets.errorToString error ])

        RemoteData.Success data ->
            viewTransactionStatus theme data


viewTransactionStatus : Theme a -> Api.TransactionStatus -> Element Msg
viewTransactionStatus theme transactionStatus =
    column
        [ centerX
        , spacing 10
        , width
            (fill
                |> maximum 500
            )
        ]
        [ text "asd" ]
