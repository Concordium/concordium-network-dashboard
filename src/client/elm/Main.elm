port module Main exposing (..)

-- import Chart

import Browser exposing (..)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Chain
import Config
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
import Explorer
import Explorer.Request
import Html exposing (Html)
import Html.Attributes exposing (style)
import Http
import Iso8601
import Json.Decode as D
import List.Extra as List
import Markdown
import Material.Icons.Sharp as Icon
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import NodeHelpers exposing (..)
import NodeSummaries exposing (..)
import Pages.ChainViz
import Pages.Graph
import Pages.Home
import Palette exposing (ColorMode(..), Palette)
import RemoteData exposing (RemoteData(..))
import Storage
import String
import Task
import Time
import Time.Extra
import Types exposing (..)
import Url exposing (Url)


port hello : String -> Cmd msg


port nodeInfo : (NetworkNode -> msg) -> Sub msg


type alias Flags =
    { width : Int, height : Int }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( chainInit, chainCmd ) =
            Chain.init Config.collector
    in
    ( { key = key
      , window = flags
      , time = Time.millisToPosix 0
      , palette = Palette.defaultDark
      , colorMode = Dark
      , currentPage = pathToPage url
      , nodes = Loading
      , sortMode = SortNone
      , selectedNode = Nothing
      , graph = { width = 800, height = 800 }
      , chainModel = chainInit
      , explorerModel = Explorer.init
      }
    , Cmd.batch
        [ hello "Hello from Elm!"
        , Cmd.map ChainMsg chainCmd
        , Cmd.map ExplorerMsg <| Explorer.Request.getConsensusStatus Explorer.ReceivedConsensusStatus
        ]
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Concordium Dashboard"
    , body =
        [ theme model.palette <|
            column
                [ width fill
                , Background.color model.palette.bg1
                , paddingEach { bottom = 60, left = 0, right = 0, top = 0 }
                ]
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
                    [ el [] (html <| Logo.concordiumLogo 24 (Palette.uiToColor ctx.palette.fg1))
                    , el [] (html <| Logo.concordiumText 110 (Palette.uiToColor ctx.palette.fg1))
                    ]
            }
        , row [ alignRight, spacing 20, Font.color ctx.palette.fg2 ]
            [ link linkstyle { url = "/", label = text "Dashboard" }
            , link linkstyle { url = "/chain", label = text "Chain" }

            -- , link linkstyle { url = "/nodegraph", label = text "Graph" }
            , viewColorModeToggle ctx
            ]
        ]


viewColorModeToggle : Context a -> Element Msg
viewColorModeToggle ctx =
    case ctx.colorMode of
        Palette.Dark ->
            el
                [ onClick ToggleDarkMode
                , mouseOver [ Font.color ctx.palette.fg1 ]
                ]
                (html <| Icon.brightness_5 14 Inherit)

        Palette.Light ->
            el
                [ onClick ToggleDarkMode
                , mouseOver [ Font.color ctx.palette.fg1 ]
                ]
                (html <| Icon.brightness_2 14 Inherit)


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
            ( model, Http.get { url = Config.collector ++ "/nodesSummary", expect = Http.expectJson FetchedNodeSummaries nodeSummariesDecoder } )

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
            ( model, Explorer.Request.getBlockInfo hash (ExplorerMsg << Explorer.ReceivedBlockInfo) )

        ToggleDarkMode ->
            case model.colorMode of
                Dark ->
                    ( { model | palette = Palette.defaultLight, colorMode = Light }, Cmd.none )

                Light ->
                    ( { model | palette = Palette.defaultDark, colorMode = Dark }, Cmd.none )

        NoopHttp r ->
            ( model, Cmd.none )

        ExplorerMsg eMsg ->
            let
                ( newExplorerModel, newExplorerCmd ) =
                    Explorer.update eMsg model.explorerModel
            in
            ( { model | explorerModel = newExplorerModel }, Cmd.map ExplorerMsg newExplorerCmd )

        BlockSummaryStubSelected stub ->
            case D.decodeString Explorer.Request.blockSummaryDecoder stub of
                Ok blockSummary ->
                    let
                        explorerModel =
                            model.explorerModel

                        newExplorerModel =
                            { explorerModel | blockSummary = Success blockSummary }
                    in
                    ( { model | explorerModel = newExplorerModel }, Cmd.none )

                Err err ->
                    let
                        x =
                            Debug.log "getBlockSummaryStub decoding" (D.errorToString err)
                    in
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
         , Time.every 2000 FetchNodeSummaries
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


theme : Palette Color -> Element msg -> Html.Html msg
theme palette elements =
    layout
        [ width fill
        , height fill
        , Background.color <| palette.bg1
        , Font.color palette.fg2
        , Font.family [ Font.typeface "IBM Plex Mono" ]
        , Font.size 14
        ]
        elements


markdown : String -> Element msg
markdown string =
    Element.html <| Markdown.toHtml [] string
