module Chain exposing (Model, Msg(..), init, update, view)

import Chain.Build as Build exposing (..)
import Chain.DictTree as DictTree exposing (DictTree)
import Chain.Flatten as Flatten exposing (DrawableChain, emptyDrawableChain)
import Chain.View as View
import Color exposing (..)
import Color.Interpolate exposing (..)
import Colors exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Keyed as Keyed
import File exposing (File)
import Grid exposing (GridSpec)
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
    , tree : DictTree ProtoBlock
    , annotatedTree : Maybe (Tree Block)
    , drawableChain : DrawableChain
    , errors : List Http.Error
    , replay : Maybe Replay
    , gridSpec : GridSpec
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = []
      , lastFinalized = Nothing
      , bestBlock = Nothing
      , tree = DictTree.init
      , annotatedTree = Nothing
      , drawableChain = Flatten.emptyDrawableChain
      , errors = []
      , replay = Nothing
      , gridSpec = spec
      }
    , Build.getNodeInfo GotNodeInfo
    )


spec : GridSpec
spec =
    { gutterWidth = 30.0
    , gutterHeight = 24.0
    , cellHeight = 36.0
    , cellWidth = 64.0
    }



-- Update


type Msg
    = GotNodeInfo (WebData (List Node))
    | LoadedHistory File
    | ReplayHistory
    | SaveHistory
    | GotHistoryString String
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNodeInfo (Success nodeInfo) ->
            ( updateChain 3 nodeInfo model, Cmd.none )

        GotNodeInfo (Failure error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        GotNodeInfo _ ->
            ( model, Cmd.none )

        SaveHistory ->
            ( model, Build.saveNodeHistory model.nodes )

        ReplayHistory ->
            ( model, Build.loadNodeHistory LoadedHistory )

        LoadedHistory historyFile ->
            ( model
            , Task.perform
                GotHistoryString
                (File.toString historyFile)
            )

        GotHistoryString historyString ->
            let
                result =
                    Decode.decodeString Build.decodeHistory historyString
            in
            case result of
                Ok history ->
                    ( { model | replay = Just { past = [], present = [], future = List.reverse history } }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Tick time ->
            case model.replay of
                Nothing ->
                    ( model, Build.getNodeInfo GotNodeInfo )

                Just replay ->
                    let
                        steppedReplay =
                            Build.advanceReplay replay
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
            List.map Build.prepareBlockSequence nodes

        shortestSeq =
            List.minimumBy List.length sequences

        newTree =
            DictTree.addAll sequences model.tree

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
                        |> DictTree.walkForwardFrom bestBlock depth
                        |> List.maximumBy Tuple.first
                        |> Maybe.withDefault ( 0, bestBlock )

                ( walkedBackward, start ) =
                    newTree
                        |> DictTree.walkBackwardFrom last (depth - 1)

                annotatedTree =
                    DictTree.buildForward depth start newTree [] Tree.tree
                        |> annotate nodes lastFinalized

                newDrawableChain =
                    Flatten.flattenTree model.gridSpec (Tuple.first lastFinalized) 2 annotatedTree
            in
            { model
                | annotatedTree = Just annotatedTree
                , nodes = updateNodes nodes model.nodes
                , tree = newTree
                , drawableChain = newDrawableChain
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
    case model.lastFinalized of
        Just lastFinalized ->
            column [ width fill, height fill ]
                [ row []
                    [ viewButton (Just SaveHistory) "Save History"
                    , viewButton (Just ReplayHistory) "Replay History"
                    ]
                , el [ centerX, centerY, spacing (round spec.gutterHeight) ]
                    (html <| View.viewChain model.gridSpec lastFinalized nodes model.drawableChain)
                ]

        Nothing ->
            none


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
