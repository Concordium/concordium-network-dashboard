port module Main exposing (main)

import Animation exposing (..)
import Browser exposing (..)
import Browser.Dom
import Browser.Events as Events
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Graph
import RewardGraph
import Task
import Time
import Types exposing (..)
import Url exposing (Url)
import View exposing (view)


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { currentTime = Time.millisToPosix 0
            , window = flags
            , key = key
            , currentPage = pathToPage url
            , graph = RewardGraph.init
            , selectedNode = Nothing
            , clock = 0
            , transfer = animation 0 |> duration 0.7
            }
    in
    ( model
    , onPageInit (pathToPage url) model
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime time ->
            ( { model
                | currentTime = time
                , transfer = animation model.clock |> duration 3000
              }
            , Cmd.none
            )

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

        NodeHovered maybeNodeId ->
            ( { model | selectedNode = maybeNodeId }, Cmd.none )

        Tick timeDelta ->
            let
                animatedGraph =
                    Graph.mapEdges
                        (\edge ->
                            { edge | animationDelta = animate model.clock model.transfer }
                        )
                        model.graph
            in
            ( { model | clock = model.clock + timeDelta, graph = animatedGraph }, Cmd.none )

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
        , Events.onAnimationFrameDelta Tick
        , Time.every 3000 CurrentTime

        -- , when
        --     (model.currentPage == SomePage)
        --     (Time.every 1000 CurrentTime)
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
