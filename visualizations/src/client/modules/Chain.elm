module Chain exposing (Model, Msg(..), dispatchMsgs, init, subscriptions, update, view)

import Browser.Events exposing (onAnimationFrameDelta)
import Chain.Build as Build exposing (..)
import Chain.DictTree as DictTree exposing (DictTree)
import Chain.Flatten as Flatten exposing (DrawableChain, emptyDrawableChain)
import Chain.Interpolate as Interpolate
import Chain.View as View
import Color exposing (..)
import Color.Interpolate exposing (..)
import Colors exposing (..)
import Context exposing (..)
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
import Transition exposing (..)
import Tree exposing (Tree)



-- Model


type alias Model =
    { nodes : List (List Node)
    , initialBlockHeight : Maybe Int
    , lastFinalized : Maybe ProtoBlock
    , bestBlock : Maybe ProtoBlock
    , tree : DictTree ProtoBlock
    , annotatedTree : Maybe (Tree Block)
    , transition : Transition DrawableChain
    , errors : List Http.Error
    , replay : Maybe Replay
    , gridSpec : GridSpec
    , blockClicked : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = []
      , initialBlockHeight = Nothing
      , lastFinalized = Nothing
      , bestBlock = Nothing
      , tree = DictTree.init
      , annotatedTree = Nothing
      , transition = Transition.constant emptyDrawableChain
      , errors = []
      , replay = Nothing
      , gridSpec = spec
      , blockClicked = Nothing
      }
    , Build.getNodeInfo GotNodeInfo
    )


spec : GridSpec
spec =
    { gutterWidth = 30.0
    , gutterHeight = 24.0
    , cellHeight = 36.0
    , cellWidth = 64.0
    , outerPadding = 64
    }



-- Update


type Msg
    = GotNodeInfo (WebData (List Node))
    | LoadedHistory File
    | ReplayHistory
    | SaveHistory
    | GotHistoryString String
    | TickSecond Posix
    | OnAnimationFrame Int
    | BlockClicked String


update : Context a -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        GotNodeInfo (Success nodeInfo) ->
            if Just nodeInfo /= List.head model.nodes then
                ( updateChain ctx 5 nodeInfo model, Cmd.none )

            else
                ( model, Cmd.none )

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
                    ( { model
                        | replay =
                            Just
                                { past = []
                                , present = []
                                , future = List.reverse history
                                }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        TickSecond time ->
            case model.replay of
                Nothing ->
                    ( model, Build.getNodeInfo GotNodeInfo )

                Just replay ->
                    let
                        steppedReplay =
                            Build.advanceReplay replay
                    in
                    update ctx
                        (GotNodeInfo (Success steppedReplay.present))
                        { model | replay = Just steppedReplay }

        OnAnimationFrame time ->
            ( { model
                | transition = Transition.step time model.transition
              }
            , Cmd.none
            )

        BlockClicked hash ->
            ( { model | blockClicked = Just hash }, Cmd.none )


type alias OutputMsgs msg =
    { onBlockClicked : String -> msg
    }


dispatchMsgs : Msg -> OutputMsgs msg -> Maybe msg
dispatchMsgs internalMsg outputMsgs =
    case internalMsg of
        BlockClicked hash ->
            Just (outputMsgs.onBlockClicked hash)

        _ ->
            Nothing


updateNodes : List Node -> List (List Node) -> List (List Node)
updateNodes new current =
    if Just new /= List.head current then
        new :: current
        --|> List.take 3 -- @TODO reenable this for production

    else
        current


updateChain : Context a -> Int -> List Node -> Model -> Model
updateChain ctx depth nodes model =
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
                    --Build.mockTree
                    DictTree.buildForward depth start newTree [] Tree.tree
                        |> annotate nodes lastFinalized

                firstBlockHeight =
                    Tree.label annotatedTree |> .blockHeight

                newDrawableChain =
                    Flatten.flattenTree ctx model.gridSpec (Tuple.first lastFinalized) 2 annotatedTree
            in
            { model
                | annotatedTree = Just annotatedTree
                , nodes = updateNodes nodes model.nodes
                , tree = newTree
                , lastFinalized = maybeLastFinalized
                , bestBlock = maybeBestBlock
                , transition =
                    let
                        oldDrawableChain =
                            model.transition |> Transition.value
                    in
                    Transition.for 800
                        (Interpolate.interpolateDrawableChain oldDrawableChain newDrawableChain)
                , initialBlockHeight =
                    Just
                        (Maybe.withDefault
                            (Tree.label annotatedTree |> .blockHeight)
                            model.initialBlockHeight
                        )
            }

        _ ->
            model



-- Subscriptione


subscriptions : Model -> Sub Msg
subscriptions model =
    if Transition.isComplete model.transition then
        Time.every 1000 TickSecond

    else
        Sub.batch
            [ Time.every 1000 TickSecond
            , Browser.Events.onAnimationFrameDelta (round >> OnAnimationFrame)
            ]



-- View


view : Context a -> Model -> Bool -> Element Msg
view ctx model showDebugButtons =
    case model.lastFinalized of
        Just lastFinalized ->
            let
                nodes =
                    model.nodes
                        |> List.uncons
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault []

                currentDrawableChain =
                    Transition.value model.transition

                vcontext =
                    { gridSpec = model.gridSpec
                    , lastFinalized = lastFinalized
                    , nodes = nodes
                    , onBlockClick = Just BlockClicked
                    , selectedBlock = model.blockClicked
                    }
            in
            column [ width fill, height fill, inFront (viewDebugButtons showDebugButtons) ]
                [ el
                    [ centerX
                    , centerY
                    , spacing (round spec.gutterHeight)
                    , inFront
                        (el [ alignLeft ]
                            (html <| View.viewCollapsedBlocksSummary ctx vcontext currentDrawableChain)
                        )
                    ]
                    (html <| View.viewChain ctx vcontext currentDrawableChain)
                ]

        Nothing ->
            none


viewDebugButtons : Bool -> Element Msg
viewDebugButtons show =
    case show of
        True ->
            row [ padding 10, spacing 10 ]
                [ viewButton (Just SaveHistory) "Save History"
                , viewButton (Just ReplayHistory) "Replay History"
                ]

        False ->
            none


viewButton : Maybe msg -> String -> Element msg
viewButton onPress label =
    button
        [ Background.color <| Element.rgba 1 1 1 0.03
        , Font.color <| Element.rgb 0.2 0.5 0.8
        , Font.size 14
        , Border.rounded 4
        , padding 10
        , mouseOver
            [ Background.color <| Element.rgba 1 1 1 0.05
            ]
        ]
        { onPress = onPress
        , label = text label
        }
