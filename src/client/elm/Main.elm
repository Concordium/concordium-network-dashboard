port module Main exposing (..)

import Browser exposing (..)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Chain
import Clipboard
import Config
import Context exposing (Context)
import Dashboard.Logo as Logo
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Explorer
import Explorer.Request
import Html exposing (Html)
import Http
import Json.Decode as D
import Json.Encode as E
import Material.Icons.Sharp as Icon
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import NodeHelpers exposing (..)
import NodeSummaries exposing (..)
import Pages.Chain
import Pages.Network
import Pages.Node
import Palette exposing (ColorMode(..), Palette)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Storage
import String
import Task
import Time
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
      , currentRoute = Route.fromUrl url
      , nodes = Loading
      , sortMode = SortNone
      , selectedNode = NotAsked
      , chainModel = chainInit
      , explorerModel = Explorer.init
      }
    , Cmd.batch
        [ Storage.loadAll ()
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
                , case model.currentRoute of
                    Network ->
                        Pages.Network.view model

                    NodeView nodeName ->
                        Pages.Node.view model

                    ChainInit ->
                        Pages.Chain.view model

                    ChainSelected hash ->
                        Pages.Chain.view model
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
            [ link linkstyle { url = "/", label = text "Network" }
            , link linkstyle { url = "/chain", label = text "Chain" }
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
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            let
                ( initModel, initCmds ) =
                    onRouteInit (Route.fromUrl url) model
            in
            ( { initModel | currentRoute = Route.fromUrl url }, initCmds )

        WindowResized width height ->
            ( { model | window = { width = width, height = height } }, Cmd.none )

        CopyToClipboard text ->
            ( model, Clipboard.copy text )

        StorageDocReceived res ->
            let
                docRes =
                    res |> D.decodeValue Storage.decodeStorageDoc
            in
            case docRes of
                Ok doc ->
                    case doc.tipe of
                        "colormode" ->
                            case D.decodeValue D.string doc.value of
                                Ok mode ->
                                    case mode of
                                        "Light" ->
                                            ( { model | palette = Palette.defaultLight, colorMode = Light }
                                            , Cmd.none
                                            )

                                        "Dark" ->
                                            ( { model | palette = Palette.defaultDark, colorMode = Dark }
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( model, Cmd.none )

                                Err e ->
                                    -- let
                                    --     x =
                                    --         Debug.log "error decoding colormode" (D.errorToString e)
                                    -- in
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err a ->
                    ( model, Cmd.none )

        TaskPerformed ->
            ( model, Cmd.none )

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
                    case model.currentRoute of
                        NodeView nodeId ->
                            -- nodeSummaries may have loaded after a nodeview URL was already open, so re-init it
                            onRouteInit newModel.currentRoute newModel

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
                    RemoteData.map (findNodeById nodeId >> Result.fromMaybe nodeId) model.nodes
              }
            , Cmd.batch [ Nav.pushUrl model.key (Route.toString (NodeView nodeId)), scrollPageToTop ]
            )

        ChainMsg chainMsg ->
            let
                ( chainModel, chainCmd ) =
                    Chain.update model chainMsg model.chainModel
            in
            ( { model | chainModel = chainModel }, Cmd.map ChainMsg chainCmd )

        BlockSelected hash ->
            ( model, Explorer.Request.getBlockInfo hash (ExplorerMsg << Explorer.ReceivedBlockInfo) )

        ToggleDarkMode ->
            case model.colorMode of
                Dark ->
                    ( { model | palette = Palette.defaultLight, colorMode = Light }
                    , Storage.save { id = "dashboard", tipe = "colormode", value = E.string "Light" }
                    )

                Light ->
                    ( { model | palette = Palette.defaultDark, colorMode = Dark }
                    , Storage.save { id = "dashboard", tipe = "colormode", value = E.string "Dark" }
                    )

        ExplorerMsg eMsg ->
            let
                ( newExplorerModel, newExplorerCmd ) =
                    Explorer.update eMsg model.explorerModel
            in
            ( { model | explorerModel = newExplorerModel }, Cmd.map ExplorerMsg newExplorerCmd )


onRouteInit : Route -> Model -> ( Model, Cmd Msg )
onRouteInit page model =
    case page of
        NodeView nodeId ->
            ( { model
                | selectedNode =
                    RemoteData.map (findNodeById nodeId >> Result.fromMaybe nodeId) model.nodes
              }
            , Cmd.none
            )

        ChainInit ->
            ( model, Cmd.none )

        ChainSelected hash ->
            ( { model | chainModel = Chain.selectBlock model.chainModel hash }
            , Explorer.Request.getBlockInfo hash (ExplorerMsg << Explorer.ReceivedBlockInfo)
            )

        _ ->
            ( model, Cmd.none )


scrollPageToTop : Cmd Msg
scrollPageToTop =
    Task.perform (\_ -> TaskPerformed) (Browser.Dom.setViewport 0 0)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ nodeInfo NodeInfoReceived
        , Browser.Events.onResize WindowResized
        , Storage.receiveDoc StorageDocReceived
        , Time.every 1000 CurrentTime
        , Time.every 2000 FetchNodeSummaries
        , case model.currentRoute of
            ChainInit ->
                Sub.map ChainMsg <| Chain.subscriptions model.chainModel

            ChainSelected hash ->
                Sub.map ChainMsg <| Chain.subscriptions model.chainModel

            _ ->
                Sub.none
        ]


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
