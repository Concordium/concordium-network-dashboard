module Chain exposing (Model, Msg(..), init, update, view)

import Chain.Api as Api exposing (..)
import Chain.DTree as DTree exposing (DTree)
import Chain.Spec exposing (..)
import Chain.SvgView as SvgView
import Chain.Tree as CTree
import Color exposing (..)
import Color.Interpolate exposing (..)
import Colors exposing (..)
import Deque exposing (Deque)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Keyed as Keyed
import File exposing (File)
import Http
import Json.Decode as Decode
import List.Extra as List
import Process exposing (sleep)
import RemoteData exposing (..)
import Task
import Time exposing (..)
import Transitions exposing (..)
import Tree exposing (Tree)



-- Model


type alias Model =
    { nodes : List (List Node)
    , lastFinalized : Maybe ProtoBlock
    , bestBlock : Maybe ProtoBlock
    , tree : DTree ProtoBlock
    , flatTree : FlattenedChain
    , animatedChain : AnimatedChain
    , annotatedTree : Maybe (Tree Block)
    , errors : List Http.Error
    , replay : Maybe Replay
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = []
      , lastFinalized = Nothing
      , bestBlock = Nothing
      , tree = DTree.init
      , flatTree = Api.emptyFlatChain
      , animatedChain = Api.emptyAnimatedChain
      , annotatedTree = Nothing
      , errors = []
      , replay = Nothing
      }
    , Api.getNodeInfo GotNodeInfo
    )



-- Update


type Msg
    = GotNodeInfo (WebData (List Node))
    | LoadedHistory File
    | ReplayHistory
    | SaveHistory
    | GotHistoryString String
    | StartAnimation Posix
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNodeInfo (Success nodeInfo) ->
            ( updateChain 3 nodeInfo model
            , Process.spawn (sleep 500)
                |> Task.andThen (\_ -> Time.now)
                |> Task.perform StartAnimation
            )

        StartAnimation time ->
            let
                chain =
                    model.animatedChain

                startedAnimatedChain =
                    { chain | stage = Animate }
            in
            ( { model | animatedChain = startedAnimatedChain }
            , Cmd.none
            )

        GotNodeInfo (Failure error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        GotNodeInfo _ ->
            ( model, Cmd.none )

        SaveHistory ->
            ( model, Api.saveNodeHistory model.nodes )

        ReplayHistory ->
            ( model, Api.loadNodeHistory LoadedHistory )

        LoadedHistory historyFile ->
            ( model
            , Task.perform
                GotHistoryString
                (File.toString historyFile)
            )

        GotHistoryString historyString ->
            let
                result =
                    Decode.decodeString Api.decodeHistory historyString
            in
            case result of
                Ok history ->
                    ( { model | replay = Just { past = [], present = [], future = List.reverse history } }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Tick time ->
            case model.replay of
                Nothing ->
                    ( model, Api.getNodeInfo GotNodeInfo )

                Just replay ->
                    let
                        steppedReplay =
                            Api.advanceReplay replay
                    in
                    update
                        (GotNodeInfo (Success steppedReplay.present))
                        { model | replay = Just steppedReplay }


updateNodes : List Node -> List (List Node) -> List (List Node)
updateNodes new current =
    if Just new /= List.head current then
        List.append [ new ] current
        --|> List.take 3

    else
        current


updateChain : Int -> List Node -> Model -> Model
updateChain depth nodes model =
    let
        maybeBestBlock =
            List.map (\node -> ( node.bestBlockHeight, node.bestBlock )) nodes
                |> List.group
                |> List.map (Tuple.mapSecond List.length)
                |> List.maximumBy Tuple.second
                |> Maybe.map Tuple.first

        sequences =
            List.map Api.prepareBlockSequence nodes

        shortestSeq =
            List.minimumBy List.length sequences

        newTree =
            DTree.addAll sequences model.tree

        maybeLastFinalized =
            shortestSeq
                |> Maybe.andThen List.uncons
                |> Maybe.map Tuple.first
    in
    case ( maybeBestBlock, maybeLastFinalized ) of
        ( Just bestBlock, Just lastFinalized ) ->
            let
                ( walkedForward, last ) =
                    newTree
                        |> DTree.walkForwardFrom bestBlock depth
                        |> List.maximumBy Tuple.first
                        |> Maybe.withDefault ( 0, bestBlock )

                ( walkedBackward, start ) =
                    newTree
                        |> DTree.walkBackwardFrom last (depth - 1)

                annotatedTree =
                    DTree.buildForward depth start newTree [] Tree.tree
                        |> annotate nodes lastFinalized

                newFlatChain =
                    flattenTree (Tuple.first lastFinalized) annotatedTree
            in
            { model
                | annotatedTree = Just annotatedTree
                , nodes = updateNodes nodes model.nodes
                , tree = newTree
                , flatTree = newFlatChain
                , animatedChain =
                    deriveAnimations
                        model.flatTree
                        newFlatChain
                        model.animatedChain.stage
                , lastFinalized = maybeLastFinalized
                , bestBlock = maybeBestBlock
            }

        _ ->
            model



-- View


view : Model -> Element Msg
view model =
    let
        nodes =
            model.nodes
                |> List.uncons
                |> Maybe.map Tuple.first
                |> Maybe.withDefault []
    in
    column [ width fill, height fill ]
        [ row []
            [ viewButton (Just SaveHistory) "Save History"
            , viewButton (Just ReplayHistory) "Replay History"
            ]
        , el [ centerX, centerY, spacing (round spec.gutterHeight) ]
            (html <| SvgView.viewAnimatedChain model.lastFinalized nodes model.animatedChain)
        ]


viewButton : Maybe msg -> String -> Element msg
viewButton onPress label =
    button
        [ Background.color <| Element.rgba 1 1 1 0.05
        , Font.color <| Element.rgb 0.2 0.5 0.8
        , Font.size 14
        , padding 10
        ]
        { onPress = onPress
        , label = text label
        }
