module Chain exposing (Model, Msg(..), init, update, view, viewFlattenedChain)

import Chain.Api as Api exposing (..)
import Chain.Connector exposing (..)
import Chain.DTree as DTree exposing (DTree)
import Chain.Spec exposing (..)
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
import RemoteData exposing (..)
import Task
import Time exposing (..)
import Transitions exposing (..)
import Tree exposing (Tree)



-- Model


type alias Model =
    { nodes : List (List Node)
    , lastFinalized : Maybe String
    , bestBlock : Maybe String
    , tree : DTree String
    , flatTree : List (Positioned Block)
    , annotatedTree : Maybe (Tree Block)
    , errors : List Http.Error
    , replay : Maybe Replay
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = []
      , lastFinalized = Nothing
      , bestBlock = Nothing
      , flatTree = []
      , annotatedTree = Nothing
      , errors = []
      , tree = DTree.init
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
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNodeInfo (Success nodeInfo) ->
            ( updateChain 6 nodeInfo model
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
            List.map .bestBlock nodes
                |> List.group
                |> List.map (Tuple.mapSecond List.length)
                |> List.maximumBy Tuple.second
                |> Maybe.map Tuple.first

        sequences =
            List.map Api.prepareBlockSequence nodes

        shortestSeq =
            List.minimumBy List.length sequences

        ntree =
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
                    ntree
                        |> DTree.walkForwardFrom bestBlock depth
                        |> List.maximumBy Tuple.first
                        |> Maybe.withDefault ( 0, bestBlock )

                ( walkedBackward, start ) =
                    ntree
                        |> DTree.walkBackwardFrom last (depth - 1)

                annotatedTree =
                    DTree.buildForward depth start ntree [] Tree.tree
                        |> annotate nodes lastFinalized
            in
            { model
                | annotatedTree = Just annotatedTree
                , nodes = updateNodes nodes model.nodes
                , tree = ntree
                , flatTree = flattenTree annotatedTree
                , lastFinalized = maybeLastFinalized
                , bestBlock = maybeBestBlock
            }

        _ ->
            model



-- View
-- New flattened view with absolute positioning, better for animation


viewFlattenedChain : List (Positioned Block) -> Element msg
viewFlattenedChain blocks =
    el
        ([ width (px 400), centerX, centerY ] ++ List.map (\b -> inFront (viewPositionedBlock b)) blocks)
        none


viewPositionedBlock : Positioned Block -> Element msg
viewPositionedBlock positionedBlock =
    Keyed.el
        [ animateAll

        --, animateFromRight
        , moveRight <|
            toFloat positionedBlock.x
                * (spec.blockWidth + spec.gutterWidth)
        , moveDown <|
            toFloat positionedBlock.y
                * (spec.blockHeight + spec.gutterHeight + spec.nodeIndicatorHeight)
        , width (px <| round (spec.blockWidth + spec.gutterWidth))
        ]
        ( positionedBlock.hash, viewBlock (unPositioned positionedBlock) )



-- Tree based view


view : Tree Block -> Element Msg
view tree =
    column [ width fill, height fill ]
        [ row []
            [ viewButton (Just SaveHistory) "Save History"
            , viewButton (Just ReplayHistory) "Replay History"
            ]
        , el [ centerX, centerY, spacing (round spec.gutterHeight) ]
            (Tree.restructure
                viewBlock
                viewChain
                tree
            )
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


viewChain : Element msg -> List (Element msg) -> Element msg
viewChain label children =
    case children of
        [] ->
            label

        _ ->
            row [ alignTop ]
                [ label
                , column [ alignTop, spacing (round spec.gutterHeight) ]
                    children
                ]


viewBlock : Block -> Element msg
viewBlock block =
    row [ alignTop ]
        [ column [ spacing 4, alignTop ]
            [ el
                [ height (px 6)
                , width (px <| round (toFloat 64 * block.percentageNodesAt))
                , Border.rounded 10
                , Background.color (toUI Colors.purple)
                , alignTop
                , growAndShrink
                ]
                none
            , el
                [ Background.color <| blockBackground block.status
                , Font.color <| blockColor block.status
                , Border.rounded 3
                , Font.size 16
                , width (px 64)
                , height (px 36)
                , alignTop
                , animateAll
                ]
                (el [ centerX, centerY ] (text block.shortHash))
            ]
        , connector spec (fromUI <| blockBackground block.status) block.connectors
        ]


viewSummaryBlock : Int -> Element msg
viewSummaryBlock n =
    el
        [ Font.color (blockBackground Candidate)
        , Border.rounded 3
        , Border.dotted
        , Border.color (blockBackground Candidate)
        , Border.width 2
        , Font.size 16
        , width (px 64)
        , height (px 36)
        ]
        (el [ centerX, centerY ] (text ("+" ++ String.fromInt n)))


blockColor : BlockStatus -> Element.Color
blockColor status =
    case status of
        Finalized ->
            toUI <| Colors.green

        LastFinalized ->
            toUI <| Colors.green

        Candidate ->
            toUI <| Colors.blue


blockBackground : BlockStatus -> Element.Color
blockBackground status =
    let
        bgAlpha =
            case status of
                LastFinalized ->
                    0.5

                _ ->
                    0.75
    in
    toUI <|
        interpolate LAB (fromUI <| blockColor status) Colors.blueishBlack bgAlpha
