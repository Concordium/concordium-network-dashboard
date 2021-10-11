module Main exposing (..)

import Analytics as Analytics
import Api
import Browser exposing (..)
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Chain
import Clipboard
import Config exposing (Config, cookiePrivacyUrl)
import Context exposing (Theme, extractTheme)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Explorer
import Explorer.View
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as D
import Json.Encode as E
import Lookup
import Material.Icons.Sharp as Icon
import Material.Icons.Types exposing (Coloring(..))
import Network exposing (viewSummaryWidgets)
import Network.Logo as Logo
import Network.Node
import Network.NodesTable
import Palette exposing (ColorMode(..), Palette)
import Process
import Route exposing (Route)
import Storage
import Task
import Time
import Url exposing (Url)
import Widgets


type alias Version =
    String


type alias Flags =
    { window : { width : Int, height : Int }
    , isProduction : Bool
    , version : Version
    , showCookieConsentBanner : Bool
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
    , timezone : Time.Zone
    , config : Config
    , window : { width : Int, height : Int }
    , version : Version
    , showCookieConsentBanner : Bool
    , palette : Palette Element.Color
    , colorMode : ColorMode
    , currentRoute : Route
    , networkModel : Network.Model
    , chainModel : Chain.Model
    , explorerModel : Explorer.Model
    , toastModel : ToastModel
    , lookupModel : Lookup.Model
    }


type Msg
    = CurrentTime Time.Posix
    | CurrentTimezone Time.Zone
    | UrlClicked UrlRequest
    | UrlChanged Url
    | WindowResized Int Int
    | CopyToClipboard String
    | ShowToast String Float
    | HideToast Int
    | StorageDocReceived D.Value
    | SetCookieConsent Bool
    | Nop -- Message triggering no operation.
      --
    | NetworkMsg Network.Msg
    | ChainMsg Chain.Msg
    | ToggleDarkMode
    | ExplorerMsg Explorer.Msg
    | LookupMsg Lookup.Msg


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
            Chain.init (chainWidthFromWidth flags.window.width)

        route =
            Route.fromUrl url

        ( initModel, initCmd ) =
            onRouteInit
                route
                { key = key
                , time = Time.millisToPosix 0
                , timezone = Time.utc
                , config = cfg
                , version = flags.version
                , showCookieConsentBanner = flags.showCookieConsentBanner
                , window = flags.window
                , palette = Palette.defaultDark
                , colorMode = Dark
                , currentRoute = route
                , networkModel = Network.init
                , chainModel = chainInit
                , explorerModel = Explorer.init
                , toastModel = { visible = False, contents = "", showCount = 0 }
                , lookupModel = Lookup.init key
                }
    in
    ( initModel
    , Cmd.batch
        [ initCmd
        , Storage.loadAll ()
        , Cmd.map ChainMsg chainCmd
        , Task.perform CurrentTimezone Time.here
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime time ->
            ( { model | time = time }, Cmd.none )

        CurrentTimezone zone ->
            ( { model | timezone = zone }, Cmd.none )

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
            ( { initModel | currentRoute = route }, Cmd.batch [ initCmds, Analytics.setPageConfig <| Route.toString route ] )

        WindowResized width height ->
            ( { model | window = { width = width, height = height } }
            , Task.perform identity (Task.succeed <| ChainMsg <| Chain.MaxWidthChanged (chainWidthFromWidth width))
            )

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

        SetCookieConsent allowed ->
            ( { model | showCookieConsentBanner = False }, Analytics.setCookieConsent allowed )

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

                updateExplorerCmd =
                    case chainMsg of
                        Chain.GotNodeInfo _ ->
                            if Chain.selectedBlockFinalizationChanged model.chainModel chainModel then
                                chainModel.selectedBlock
                                    |> Maybe.map
                                        (\hash ->
                                            Api.getBlockInfo
                                                hash
                                                (ExplorerMsg << Explorer.ReceivedBlockResponse)
                                        )
                                    |> Maybe.withDefault Cmd.none

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | chainModel = chainModel }
            , Cmd.batch
                [ Cmd.map ChainMsg chainCmd
                , updateExplorerCmd
                ]
            )

        ToggleDarkMode ->
            (case model.colorMode of
                Dark ->
                    ( { model | palette = Palette.defaultLight, colorMode = Light }
                    , Storage.save { id = "dashboard", tipe = "colormode", value = E.string "Light" }
                    )

                Light ->
                    ( { model | palette = Palette.defaultDark, colorMode = Dark }
                    , Storage.save { id = "dashboard", tipe = "colormode", value = E.string "Dark" }
                    )
            )
                |> rebuildChain

        ExplorerMsg explorerMsg ->
            let
                ( newExplorerModel, newExplorerCmd ) =
                    Explorer.update explorerMsg model.explorerModel

                chainModel =
                    case explorerMsg of
                        Explorer.ReceivedConsensusStatus res ->
                            let
                                maybeBestBlock =
                                    res
                                        |> Result.toMaybe
                                        |> Maybe.map .bestBlock

                                oldChainModel =
                                    model.chainModel
                            in
                            { oldChainModel | selectedBlock = maybeBestBlock }

                        _ ->
                            model.chainModel
            in
            ( { model | explorerModel = newExplorerModel, chainModel = chainModel }, Cmd.map ExplorerMsg newExplorerCmd )

        LookupMsg lookupMsg ->
            case lookupMsg of
                Lookup.CopyToClipboard str ->
                    update (CopyToClipboard str) model

                _ ->
                    let
                        ( newLookupModel, cmd ) =
                            Lookup.update lookupMsg model.lookupModel
                    in
                    ( { model | lookupModel = newLookupModel }, Cmd.map LookupMsg cmd )

        Nop ->
            ( model, Cmd.none )


rebuildChain : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
rebuildChain ( model, cmd ) =
    let
        ( chainModel, chainCmd ) =
            Chain.rebuild model model.chainModel
    in
    ( { model | chainModel = chainModel }, Cmd.batch [ cmd, Cmd.map ChainMsg chainCmd ] )


chainWidthFromWidth : Int -> Int
chainWidthFromWidth width =
    min (width - 100) 1100


onRouteInit : Route -> Model -> ( Model, Cmd Msg )
onRouteInit page model =
    case page of
        Route.Chain Nothing ->
            ( model
            , Api.getConsensusStatus (ExplorerMsg << Explorer.ReceivedConsensusStatus)
            )

        Route.Chain (Just hash) ->
            ( { model | chainModel = Chain.selectBlock model.chainModel hash }
            , Api.getBlockInfo hash (ExplorerMsg << Explorer.ReceivedBlockResponse)
            )

        Route.Lookup hash ->
            let
                ( newModel, cmd ) =
                    Lookup.onRouteInit hash model.lookupModel
            in
            ( { model | lookupModel = newModel }, Cmd.map LookupMsg cmd )

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
        [ themeLayout model.palette <|
            el
                ([ width fill
                 , height fill
                 ]
                    ++ (if model.showCookieConsentBanner then
                            [ inFront <| cookieConsentBanner (extractTheme model) ]

                        else
                            []
                       )
                )
            <|
                el
                    [ scrollbarY
                    , width fill
                    ]
                <|
                    column
                        [ width fill
                        , clipX
                        , paddingEach { bottom = 60, left = 0, right = 0, top = 0 }
                        ]
                        [ viewTopNavigation (extractTheme model) model.version model.currentRoute
                        , case model.currentRoute of
                            Route.Network ->
                                Element.map NetworkMsg <| Network.NodesTable.view model model.networkModel

                            Route.NodeView _ ->
                                Element.map NetworkMsg <| Network.Node.view model model.networkModel

                            Route.Chain _ ->
                                viewChainPage model

                            Route.Lookup _ ->
                                viewLookupPage model
                        ]
        ]
    }


viewTopNavigation : Theme a -> Version -> Route -> Element Msg
viewTopNavigation ctx version currentRoute =
    let
        linkstyle active =
            [ mouseOver [ Font.color ctx.palette.fg1 ]
            , if active then
                Font.color ctx.palette.fg1

              else
                Font.color ctx.palette.fg2
            ]
    in
    row [ width fill, paddingXY 30 20 ]
        [ link []
            { url = "/"
            , label =
                row [ spacing 12 ]
                    [ el [] (html <| Logo.concordiumLogo 24 (Palette.uiToColor ctx.palette.fg1))
                    , el [] (html <| Logo.concordiumText 110 (Palette.uiToColor ctx.palette.fg1))
                    , el [ Font.color ctx.palette.fg3 ] <| text version
                    ]
            }
        , row [ alignRight, spacing 20 ]
            [ link (linkstyle <| Route.isNetwork currentRoute) { url = Route.toString Route.Network, label = text "Network" }
            , link (linkstyle <| Route.isChain currentRoute) { url = Route.toString <| Route.Chain Nothing, label = text "Chain" }
            , link (linkstyle <| Route.isLookup currentRoute) { url = Route.toString <| Route.Lookup Nothing, label = text "Lookup" }
            , viewColorModeToggle ctx
            ]
        ]


cookieConsentBanner : Theme a -> Element Msg
cookieConsentBanner theme =
    column
        [ padding 20
        , alignBottom
        , width fill
        , spacing 10
        , Background.color theme.palette.bg3
        ]
        [ paragraph [] [ text "We use cookies to ensure that we give you the best experience on our website, but only if you grant us the permission" ]
        , row [ spacing 10 ]
            [ el (buttonAttributes theme ++ [ onClick <| SetCookieConsent True ]) <| text "Allow"
            , el (buttonAttributes theme ++ [ onClick <| SetCookieConsent False ]) <| text "Disallow"
            , link [ onClick <| UrlClicked <| Browser.External cookiePrivacyUrl ]
                { url = cookiePrivacyUrl
                , label = el [ Font.underline ] <| text "Privacy policy"
                }
            ]
        ]


buttonAttributes : Theme a -> List (Attribute msg)
buttonAttributes theme =
    [ padding 10, Border.color theme.palette.fg3, Border.width 1, Border.rounded 5, pointer, mouseOver [ Background.color theme.palette.bg2 ] ]


viewColorModeToggle : Theme a -> Element Msg
viewColorModeToggle ctx =
    let
        icon =
            case ctx.colorMode of
                Palette.Dark ->
                    html <| Icon.brightness_5 14 Inherit

                Palette.Light ->
                    html <| Icon.brightness_2 14 Inherit
    in
    el
        [ onClick ToggleDarkMode
        , mouseOver [ Font.color ctx.palette.fg1 ]
        ]
        icon


viewChainPage : Model -> Element Msg
viewChainPage model =
    let
        showDevTools =
            not <| Config.isProduction model.config

        theme =
            extractTheme model
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
                (Element.map ChainMsg <| Chain.view theme model.chainModel showDevTools)
            , row [ viewCopiedToast model, centerX ]
                [ Element.map translateMsg <|
                    Explorer.View.view theme model.timezone model.explorerModel
                ]
            ]


viewLookupPage : Model -> Element Msg
viewLookupPage model =
    Widgets.content <|
        column [ width fill, height fill, spacing 20 ]
            [ viewSummaryWidgets model model.networkModel.nodes
            , row [ viewCopiedToast model, centerX ] []
            , Element.map LookupMsg <|
                Lookup.view
                    { palette = model.palette
                    , colorMode = model.colorMode
                    }
                    model.timezone
                    model.lookupModel
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

        Explorer.View.Display displayMsg ->
            ExplorerMsg <| Explorer.Display displayMsg

        Explorer.View.UrlClicked link ->
            UrlClicked link

        Explorer.View.Nop ->
            Nop


themeLayout : Palette Color -> Element msg -> Html.Html msg
themeLayout palette elements =
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Just palette.fg1
                , backgroundColor = Just palette.bg2
                , shadow = Nothing
                }
            ]
        }
        [ width fill
        , height fill
        , Background.color <| palette.bg1
        , Font.color palette.fg2
        , Font.family [ Font.typeface "IBM Plex Mono" ]
        , Font.size 14
        ]
        elements
