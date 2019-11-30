module Main exposing (main)

import Animation exposing (..)
import Browser exposing (..)
import Browser.Dom
import Browser.Events as Events
import Browser.Navigation as Nav exposing (Key)
import Chain exposing (viewFlattenedChain)
import Chain.Spec exposing (spec)
import Curve.ParameterValue exposing (value)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Graph
import RewardGraph exposing (updateEdgeInterval, updateEdgeValue)
import Task
import Time
import Types exposing (..)
import Url exposing (Url)
import Widgets exposing (..)


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( chainModel, chainCmd ) =
            Chain.init

        model =
            { currentTime = Time.millisToPosix 0
            , window = flags
            , key = key
            , currentPage = pathToPage url
            , graph = RewardGraph.init
            , selectedNode = Nothing
            , clock = 0
            , transfer = animation 0 |> duration 0.7
            , ticks = 0
            , chainModel = chainModel
            }
    in
    ( model
    , Cmd.batch [ onPageInit (pathToPage url) model, Cmd.map ChainMsg chainCmd ]
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Concordium Visualizations"
    , body =
        [ theme <|
            case model.currentPage of
                Home ->
                    [ row
                        [ width fill
                        , height (px model.window.height)
                        ]
                        (Maybe.map (Chain.view >> List.singleton) model.chainModel.annotatedTree
                            |> Maybe.withDefault []
                        )
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

        NodeHovered maybeNodeId ->
            ( { model | selectedNode = maybeNodeId }, Cmd.none )

        CurrentTime time ->
            ( { model
                | currentTime = time
              }
            , Cmd.none
            )

        Tick timeDelta ->
            let
                animatedGraph =
                    Graph.mapEdges
                        (\edge ->
                            let
                                updatedAnimation =
                                    if isDone model.clock edge.animation then
                                        animation model.clock
                                            |> duration (RewardGraph.millisecondsFromInterval edge.interval)

                                    else
                                        edge.animation
                            in
                            { edge
                                | animationValue = animate model.clock edge.animation
                                , animation = updatedAnimation
                            }
                        )
                        model.graph

                updatedGraph =
                    animatedGraph |> RewardGraph.tick model.ticks
            in
            ( { model
                | clock = model.clock + timeDelta
                , ticks = model.ticks + 1
                , graph = updatedGraph
              }
            , Cmd.none
            )

        EdgeValueChanged originId targetId valueString ->
            let
                updatedGraph =
                    updateEdgeValue originId targetId valueString model.graph
            in
            ( { model | graph = updatedGraph }, Cmd.none )

        EdgeIntervalChanged originId targetId intervalString ->
            let
                updatedGraph =
                    updateEdgeInterval originId targetId intervalString model.graph
            in
            ( { model | graph = updatedGraph }, Cmd.none )

        ChainMsg chainMsg ->
            let
                ( subModel, subCmd ) =
                    Chain.update chainMsg model.chainModel
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
        , Time.every 500 (ChainMsg << Chain.Tick)

        --, Events.onAnimationFrameDelta Tick
        --, Time.every 3000 CurrentTime
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
