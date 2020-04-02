module Main exposing (main)

import Animation exposing (..)
import Browser exposing (..)
import Browser.Dom
import Browser.Events as Events
import Browser.Navigation as Nav exposing (Key)
import Chain
import Colors
import Context exposing (Context)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Graph exposing (Graph)
import Html exposing (Html)
import Page exposing (..)
import Palette exposing (..)
import RewardGraph exposing (..)
import Task
import Time exposing (Posix)
import Url exposing (Url)

nodeInfoEndpoint = 
    "https://dashboard.eu.staging.concordium.com/nodesBlocksInfo"


type alias Flags =
    { width : Int, height : Int }


type alias Window =
    { width : Int, height : Int }


type alias Model =
    { currentTime : Time.Posix
    , window : Window
    , key : Key
    , palette : Palette Color
    , time : Posix
    , currentPage : Page
    , chainModel : Chain.Model
    }


type Msg
    = CurrentTime Time.Posix
    | UrlClicked UrlRequest
    | UrlChanged Url
    | OpenPage Page
    | OpenUrl String
    | WindowResized Int Int
    | ChainMsg Chain.Msg
    | Noop


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( chainModel, chainCmd ) =
            Chain.init nodeInfoEndpoint

        model =
            { currentTime = Time.millisToPosix 0
            , window = flags
            , key = key
            , palette = Palette.defaultDark
            , time = Time.millisToPosix 0
            , currentPage = pathToPage url
            , chainModel = chainModel
            }
    in
    ( model
    , Cmd.batch [ onPageInit (pathToPage url) model, Cmd.map ChainMsg chainCmd ]
    )


theme : Context a -> List (Element msg) -> Html.Html msg
theme ctx x =
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ width fill
        , Background.color <| ctx.palette.bg1
        , Font.color <| ctx.palette.fg1
        , Font.size 16
        , Font.regular
        , Font.family
            [ Font.typeface "Poppins"
            , Font.sansSerif
            ]
        ]
        (column [ width fill, spacing 20 ]
            x
        )


view : Model -> Browser.Document Msg
view model =
    { title = "Concordium Visualizations"
    , body =
        [ theme model <|
            case model.currentPage of
                Home ->
                    [ el
                        [ width fill
                        , height (px model.window.height)
                        ]
                        (Chain.view model model.chainModel True)
                        |> Element.map ChainMsg
                    ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)
                        , onPageExit model.currentPage model
                        , onPageInit (pathToPage url) model
                        ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( { model | currentPage = pathToPage url }, scrollPageToTop )

        OpenPage page ->
            ( { model | currentPage = page }
            , Cmd.batch [ Nav.pushUrl model.key (pageToPath page), scrollPageToTop ]
            )

        OpenUrl url ->
            ( model, Nav.load url )

        WindowResized width height ->
            ( { model | window = { width = width, height = height } }, Cmd.none )

        CurrentTime time ->
            ( { model
                | currentTime = time
              }
            , Cmd.none
            )

        ChainMsg chainMsg ->
            let
                ( subModel, subCmd ) =
                    Chain.update model chainMsg model.chainModel
            in
            ( { model | chainModel = subModel }, Cmd.map ChainMsg subCmd )

        Noop ->
            ( model, Cmd.none )


onPageInit page model =
    case page of
        _ ->
            Cmd.none


onPageExit page model =
    case page of
        _ ->
            Cmd.none


scrollPageToTop =
    Task.perform (\_ -> Noop) (Browser.Dom.setViewport 0 0)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResized
        , Sub.map ChainMsg (Chain.subscriptions model.chainModel)
        ]


when bool sub =
    if bool then
        sub

    else
        Sub.none


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
