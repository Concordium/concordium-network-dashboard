module Main exposing (..)

import Browser exposing (..)
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Chain
import Clipboard
import Config exposing (Config)
import Context exposing (Context)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Explorer
import Explorer.Request
import Explorer.View
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as D
import Json.Encode as E
import Material.Icons.Sharp as Icon
import Material.Icons.Types exposing (Coloring(..))
import Network exposing (viewSummaryWidgets)
import Network.Logo as Logo
import Network.Node
import Network.NodesTable
import Palette exposing (ColorMode(..), Palette)
import Process
import Route exposing (Route(..))
import Storage
import Task
import Time
import Url exposing (Url)
import Widgets


type alias Flags =
    { window : { width : Int, height : Int }
    , isProduction : Bool
    }


{-| Model for the notification toast.
Note that visibility needs to be changeable independently from contents
(i.e. they cannot be replaced by a single Maybe) because the visibility is animated:
The text would change while the toast is still visible.
The showCount field is used by the HideToast message to cancel the hiding
if another toast message has been set since the hide was scheduled.
This is the only way to do it as timeouts cannot be cancelled.
-}
type alias ToastModel =
    { visible : Bool
    , contents : String
    , showCount : Int
    }


type alias Model =
    { key : Key
    , time : Time.Posix
    , config : Config
    , window : { width : Int, height : Int }
    , palette : Palette Element.Color
    , colorMode : ColorMode
    , currentRoute : Route
    , networkModel : Network.Model
    , chainModel : Chain.Model
    , explorerModel : Explorer.Model
    , toastModel : ToastModel
    }


type Msg
    = CurrentTime Time.Posix
    | UrlClicked UrlRequest
    | UrlChanged Url
    | WindowResized Int Int
    | CopyToClipboard String
    | ShowToast String Float
    | HideToast Int
    | StorageDocReceived D.Value
      --
    | NetworkMsg Network.Msg
    | ChainMsg Chain.Msg
    | ToggleDarkMode
    | ExplorerMsg Explorer.Msg


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


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        cfg =
            Config.defaultConfig (Config.parseEnv flags.isProduction)

        ( chainInit, chainCmd ) =
            Chain.init cfg.collectorUrl

        route =
            Route.fromUrl url

        ( initModel, initCmd ) =
            onRouteInit
                route
                { key = key
                , time = Time.millisToPosix 0
                , config = cfg
                , window = flags.window
                , palette = Palette.defaultDark
                , colorMode = Dark
                , currentRoute = route
                , networkModel = Network.init { collectorUrl = cfg.collectorUrl }
                , chainModel = chainInit
                , explorerModel = Explorer.init { middlewareUrl = cfg.middlewareUrl }
                , toastModel = { visible = False, contents = "", showCount = 0 }
                }
    in
    ( initModel
    , Cmd.batch
        [ initCmd
        , Storage.loadAll ()
        , Cmd.map ChainMsg chainCmd
        ]
    )


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
                route =
                    Route.fromUrl url

                ( initModel, initCmds ) =
                    onRouteInit route model
            in
            ( { initModel | currentRoute = route }, initCmds )

        WindowResized width height ->
            ( { model | window = { width = width, height = height } }, Cmd.none )

        CopyToClipboard text ->
            let
                ( newModel, cmd ) =
                    update (ShowToast text 5000) model
            in
            ( newModel, Cmd.batch [ Clipboard.copy text, cmd ] )

        ShowToast text hideDelayMs ->
            let
                newShowCount =
                    model.toastModel.showCount + 1
            in
            ( { model | toastModel = { visible = True, contents = text, showCount = newShowCount } }
            , Process.sleep hideDelayMs |> Task.perform (\_ -> HideToast newShowCount)
            )

        HideToast showCount ->
            if showCount /= model.toastModel.showCount then
                -- Noop: Another toast message has been set (and another process for hiding it is waiting to get triggered).
                ( model, Cmd.none )

            else
                let
                    toastModel =
                        model.toastModel
                in
                ( { model | toastModel = { toastModel | visible = False } }, Cmd.none )

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

                                Err err ->
                                    -- let
                                    --     x =
                                    --         Debug.log "error decoding colormode" (D.errorToString e)
                                    -- in
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        NetworkMsg networkMsg ->
            let
                ( newNetworkModel, newNetworkCmd ) =
                    Network.update networkMsg model.networkModel model.currentRoute model.key
            in
            ( { model | networkModel = newNetworkModel }, Cmd.map NetworkMsg newNetworkCmd )

        ChainMsg chainMsg ->
            let
                ( chainModel, chainCmd ) =
                    Chain.update model chainMsg model.chainModel
            in
            ( { model | chainModel = chainModel }, Cmd.map ChainMsg chainCmd )

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

        ExplorerMsg explorerMsg ->
            let
                ( newExplorerModel, newExplorerCmd ) =
                    Explorer.update explorerMsg model.explorerModel
            in
            ( { model | explorerModel = newExplorerModel }, Cmd.map ExplorerMsg newExplorerCmd )


onRouteInit : Route -> Model -> ( Model, Cmd Msg )
onRouteInit page model =
    case page of
        ChainInit ->
            ( model
            , Explorer.Request.getConsensusStatus model.explorerModel.config (ExplorerMsg << Explorer.ReceivedConsensusStatus)
            )

        ChainSelected hash ->
            ( { model | chainModel = Chain.selectBlock model.chainModel hash }
            , Explorer.Request.getBlockInfo model.explorerModel.config hash (ExplorerMsg << Explorer.ReceivedBlockInfo)
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Storage.receiveDoc StorageDocReceived
        , Time.every 1000 CurrentTime
        , Sub.map NetworkMsg <| Network.subscriptions
        , if Route.isChain model.currentRoute then
            Sub.map ChainMsg <| Chain.subscriptions model.chainModel

          else
            Sub.none
        ]


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
                        Element.map NetworkMsg <| Network.NodesTable.view model model.networkModel

                    NodeView nodeName ->
                        Element.map NetworkMsg <| Network.Node.view model model.networkModel

                    ChainInit ->
                        viewChain model

                    ChainSelected hash ->
                        viewChain model
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
            [ link linkstyle { url = Route.toString Network, label = text "Network" }
            , link linkstyle { url = Route.toString ChainInit, label = text "Chain" }
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


viewChain : Model -> Element Msg
viewChain model =
    let
        showDevTools =
            not <| Config.isProduction model.config
    in
    Widgets.content <|
        column [ width fill, height fill, spacing 20 ]
            [ viewSummaryWidgets model model.networkModel.nodes
            , el
                [ width fill
                , height (fill |> minimum 200)
                , Background.color <| Palette.darkish model.palette.bg2
                , Border.color model.palette.bg2
                , Border.rounded 6
                , Border.width 1
                ]
                (Element.map ChainMsg <| Chain.view model model.chainModel showDevTools)
            , row [ viewCopiedToast model, centerX ]
                [ Element.map translateMsg <|
                    Explorer.View.view model model.explorerModel
                ]
            ]


viewCopiedToast : Model -> Attribute Msg
viewCopiedToast model =
    let
        fadingAttributes =
            [ transparent <| not <| model.toastModel.visible
            , htmlAttribute <| style "transition" "opacity 200ms ease-out 200ms"
            ]
    in
    inFront <|
        el
            ([ width (fill |> maximum 800)
             , height (fill |> maximum 45)
             , Background.color <| Palette.darkish model.palette.bg2
             , Border.color model.palette.fg3
             , Border.rounded 6
             , Border.width 1
             , centerX
             , Font.center
             , paddingXY 0 14
             , moveUp 490
             ]
                ++ fadingAttributes
            )
            (text <| "Copied: \"" ++ model.toastModel.contents ++ "\"")


translateMsg : Explorer.View.Msg -> Msg
translateMsg msg =
    case msg of
        Explorer.View.CopyToClipboard str ->
            CopyToClipboard str

        Explorer.View.BlockClicked block ->
            ChainMsg (Chain.BlockClicked block)


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
