port module Main exposing (..)

-- import Chart

import Browser exposing (..)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Chain
import Context exposing (Context)
import Dashboard.Formatting exposing (..)
import Dashboard.Logo as Logo
import Dashboard.Widgets exposing (..)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import Http
import Iso8601
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import List.Extra as List
import Markdown
import Material.Icons.Sharp as Icon
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import NodeHelpers exposing (..)
import Pages.ChainViz
import Pages.Graph
import Pages.Home
import Palette exposing (ColorMode(..), Palette)
import RemoteData exposing (RemoteData(..))
import String
import Task
import Time
import Time.Distance exposing (inWordsWithConfig)
import Time.Distance.I18n as I18n
import Time.Extra
import Types exposing (..)
import Url exposing (Url)


port hello : String -> Cmd msg


port nodeInfo : (NetworkNode -> msg) -> Sub msg


nodeSummariesDecoder =
    D.list
        (D.succeed NetworkNode
            |> required "nodeName" D.string
            |> required "nodeId" D.string
            -- @TODO make this mandatory when collector has been deployed
            |> optional "peerType" D.string "Unknown"
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
            |> optional "consensusRunning" D.bool False
            |> optional "bakingCommitteeMember" D.bool False
            |> optional "finalizationCommitteeMember" D.bool False
        )


type alias Flags =
    { width : Int, height : Int }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( chainInit, chainCmd ) =
            Chain.init
    in
    ( { key = key
      , window = flags
      , time = Time.millisToPosix 0
      , palette = Palette.defaultLight
      , colorMode = Light
      , currentPage = pathToPage url
      , nodes = Loading
      , sortMode = SortNone
      , selectedNode = Nothing
      , graph = { width = 800, height = 800 }
      , chainModel = chainInit
      }
    , Cmd.batch
        [ hello "Hello from Elm!"
        , Cmd.map ChainMsg chainCmd
        ]
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Concordium Dashboard"
    , body =
        [ theme model.palette <|
            [ column [ width fill ]
                [ viewHeader model
                , case model.currentPage of
                    Dashboard ->
                        Pages.Home.view model

                    NodeGraph ->
                        Pages.Graph.view model

                    NodeView nodeName ->
                        Pages.Graph.view model

                    ChainViz ->
                        Pages.ChainViz.view model
                ]
            ]
        ]
    }


viewHeader : Context a -> Element Msg
viewHeader ctx =
    let
        linkstyle =
            [ mouseOver [ Font.color ctx.palette.fg1 ] ]
    in
    row [ width fill, height (px 70), paddingXY 30 0 ]
        [ link []
            { url = "/"
            , label =
                row [ spacing 12 ]
                    [ el [] (html <| Logo.concordiumLogo 24 (Palette.uiToColor ctx.palette.c1))
                    , el [] (html <| Logo.concordiumText 110 (Palette.uiToColor ctx.palette.fg1))
                    ]
            }
        , row [ alignRight, spacing 20, Font.color ctx.palette.fg2 ]
            [ link linkstyle { url = "/", label = text "Dashboard" }
            , link linkstyle { url = "/chain", label = text "Chain" }
            , link linkstyle { url = "/nodegraph", label = text "Graph" }
            , el [ onClick ToggleDarkMode ] (html <| Icon.brightness_7 16 Inherit)
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime time ->
            ( { model | time = time }, Cmd.none )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    let
                        ( initModel, initCmds ) =
                            onPageInit (pathToPage url) model
                    in
                    ( initModel
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)

                        -- , onPageExit model.currentPage model
                        , initCmds
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
            ( { model | nodes = RemoteData.map (Dict.insert node.nodeId node) model.nodes }, Cmd.none )

        FetchNodeSummaries _ ->
            ( model, Http.get { url = "https://dashboard.eu.test.concordium.com/nodesSummary", expect = Http.expectJson FetchedNodeSummaries nodeSummariesDecoder } )

        FetchedNodeSummaries r ->
            case r of
                Ok nodeSummaries ->
                    let
                        newModel =
                            { model
                                | nodes = Success nodes
                            }

                        nodes =
                            nodeSummaries
                                |> List.map (\node -> ( node.nodeId, node ))
                                |> Dict.fromList
                    in
                    case model.currentPage of
                        NodeView nodeId ->
                            -- nodeSummaries may have loaded after a nodeview URL was already open, so re-init it
                            onPageInit newModel.currentPage newModel

                        _ ->
                            ( newModel, Cmd.none )

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
            ( { model
                | selectedNode =
                    model.nodes
                        |> RemoteData.map (findNodeById nodeId)
                        |> RemoteData.toMaybe
                        |> Maybe.join
              }
            , Cmd.batch [ Nav.pushUrl model.key (pageToPath (NodeView nodeId)), scrollPageToTop ]
            )

        GraphZoom zoom ->
            ( { model | graph = { width = model.graph.width + zoom, height = model.graph.height + zoom } }, Cmd.none )

        DevResetCache ->
            ( model, Http.get { url = "/dev/reset", expect = Http.expectWhatever NoopHttp } )

        ChainMsg chainMsg ->
            let
                ( chainModel, chainCmd ) =
                    Chain.update model chainMsg model.chainModel
            in
            ( { model | chainModel = chainModel }, Cmd.map ChainMsg chainCmd )
                |> triggerOnDispatch (Chain.dispatchMsgs chainMsg { onBlockClicked = BlockSelected })

        BlockSelected hash ->
            ( model, Cmd.none )

        ToggleDarkMode ->
            case model.colorMode of
                Dark ->
                    ( { model | palette = Palette.defaultLight, colorMode = Light }, Cmd.none )

                Light ->
                    ( { model | palette = Palette.defaultDark, colorMode = Dark }, Cmd.none )

        NoopHttp r ->
            ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


triggerOnDispatch : Maybe Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
triggerOnDispatch maybeMsg ( currentModel, currentCmd ) =
    case maybeMsg of
        Just message ->
            ( currentModel
            , Cmd.batch
                [ currentCmd
                , Task.perform identity (Task.succeed message)
                ]
            )

        Nothing ->
            ( currentModel, currentCmd )


onPageInit : Page -> Model -> ( Model, Cmd Msg )
onPageInit page model =
    case page of
        NodeView nodeId ->
            ( { model
                | selectedNode =
                    model.nodes
                        |> RemoteData.map (findNodeById nodeId)
                        |> RemoteData.toMaybe
                        |> Maybe.join
              }
            , Cmd.none
            )

        NodeGraph ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )


onPageExit page model =
    case page of
        _ ->
            Cmd.none


scrollPageToTop =
    Task.perform (\_ -> Noop) (Browser.Dom.setViewport 0 0)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ nodeInfo NodeInfoReceived
         , Browser.Events.onResize WindowResized
         , Time.every 1000 CurrentTime
         , Time.every 1000 FetchNodeSummaries
         ]
            ++ (case model.currentPage of
                    ChainViz ->
                        [ Sub.map ChainMsg <| Chain.subscriptions model.chainModel ]

                    _ ->
                        []
               )
        )


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


theme : Palette Color -> List (Element msg) -> Html.Html msg
theme palette x =
    layout
        [ width fill
        , height fill
        , Background.color <| palette.bg1
        , Font.color palette.fg1
        , Font.family [ Font.typeface "IBM Plex Mono" ]
        , Font.size 14
        ]
    <|
        column [ width fill, height fill ]
            x


markdown : String -> Element msg
markdown string =
    Element.html <| Markdown.toHtml [] string
