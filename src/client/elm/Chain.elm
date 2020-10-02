module Chain exposing
    ( Model
    , Msg(..)
    , init
    , selectBlock
    , selectedBlockFinalizationChanged
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Browser.Navigation as Nav
import Chain.Build as Build exposing (..)
import Chain.DictTree as DictTree exposing (DictTree)
import Chain.Flatten as Flatten exposing (DrawableChain, emptyDrawableChain)
import Chain.Grid exposing (GridSpec)
import Chain.Interpolate as Interpolate
import Chain.Transition as Transition exposing (..)
import Chain.View as View
import CollectionHelpers exposing (maxFrequency)
import Context exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import File exposing (File)
import Http
import Json.Decode as Decode
import List.Extra as List
import RemoteData exposing (..)
import Route exposing (Route(..))
import Set
import Task
import Time exposing (..)
import Tree exposing (Tree)



-- Model


type alias Model =
    { endpoint : String
    , nodes : List (List Node)
    , initialBlockHeight : Maybe Int
    , lastFinalized : Maybe ProtoBlock
    , bestBlock : Maybe ProtoBlock
    , tree : DictTree ProtoBlock
    , annotatedTree : Maybe (Tree Block)
    , transition : Transition DrawableChain
    , errors : List Http.Error
    , replay : Maybe Replay
    , gridSpec : Maybe GridSpec
    , selectedBlock : Maybe String
    }


init : String -> ( Model, Cmd Msg )
init collectorEndpoint =
    ( { endpoint = collectorEndpoint
      , nodes = []
      , initialBlockHeight = Nothing
      , lastFinalized = Nothing
      , bestBlock = Nothing
      , tree = DictTree.init
      , annotatedTree = Nothing
      , transition = Transition.constant emptyDrawableChain
      , errors = []
      , replay = Nothing
      , gridSpec = Nothing
      , selectedBlock = Nothing
      }
    , Build.getNodeInfo collectorEndpoint GotNodeInfo
    )


spec : Int -> GridSpec
spec offset =
    { gutterWidth = 30.0
    , gutterHeight = 24.0
    , cellHeight = 36.0
    , cellWidth = 64.0
    , outerPadding = 64
    , initialOffsetX = offset
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
                    ( model
                    , Build.getNodeInfo model.endpoint GotNodeInfo
                    )

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
            ( model, Nav.pushUrl ctx.key (Route.toString <| ChainSelected hash) )


selectBlock : Model -> String -> Model
selectBlock model hash =
    { model | selectedBlock = Just hash }


selectedBlockFinalizationChanged : Model -> Model -> Bool
selectedBlockFinalizationChanged lastModel currentModel =
    if lastModel.selectedBlock == currentModel.selectedBlock then
        Maybe.map3
            (\selectedBlockHeight lastFinalizedHeight lastModelLastFinalizedHeight ->
                lastModelLastFinalizedHeight /= lastFinalizedHeight && selectedBlockHeight <= lastFinalizedHeight
            )
            (getSelectedBlockHeight lastModel)
            (Maybe.map Tuple.first currentModel.lastFinalized)
            (Maybe.map Tuple.first lastModel.lastFinalized)
            |> Maybe.withDefault False

    else
        False


getSelectedBlockHeight : Model -> Maybe Int
getSelectedBlockHeight model =
    model.selectedBlock
        |> Maybe.andThen
            (getBlockHeight
                (model.nodes
                    |> List.head
                    |> Maybe.withDefault []
                )
            )


{-| Note: Only searches blocks back to last best block
-}
getBlockHeight : List Node -> String -> Maybe Int
getBlockHeight nodes hash =
    nodes
        |> List.map (Build.prepareBlockSequence >> List.filter (\proto -> Tuple.second proto == hash))
        |> List.concat
        |> Set.fromList
        |> (\candidates ->
                case Set.toList candidates of
                    [ single ] ->
                        Just (Tuple.first single)

                    _ ->
                        Nothing
           )


updateNodes : List Node -> List (List Node) -> List (List Node)
updateNodes new current =
    if Just new /= List.head current then
        new
            :: current
            |> List.take 3

    else
        current


updateChain : Context a -> Int -> List Node -> Model -> Model
updateChain ctx depth nodes model =
    let
        -- The best block according to the majority of the nodes.
        maybeBestBlock =
            nodes
                |> List.map (\node -> ( node.bestBlockHeight, node.bestBlock ))
                |> maxFrequency

        -- The finalized block with the highest height.
        maybeLastFinalized =
            nodes
                |> List.map (\node -> ( node.finalizedBlockHeight, node.finalizedBlock ))
                |> maxFrequency

        -- Block sequences for all nodes.
        sequences =
            List.map Build.prepareBlockSequence nodes

        -- Merge sequences into existing dict-based model.
        newTree =
            DictTree.addAll sequences model.tree
    in
    case ( maybeBestBlock, maybeLastFinalized ) of
        ( Just bestBlock, Just ( lastFinalizedHeight, lastFinalizedBlock ) ) ->
            let
                -- Find deepmost blocks of all branches up to a depth of `depth` from the best block
                -- according to the majority of the nodes.
                -- The reason for not going beyond that is to ensure that this block remains included in the view.
                ( stepsWalkedForwards, last ) =
                    newTree
                        |> DictTree.walkForwardFrom bestBlock depth
                        |> List.maximumBy Tuple.first
                        |> Maybe.withDefault ( 0, bestBlock )

                -- Walk back from the last block to find the start block.
                ( stepsWalkedBackwards, start ) =
                    newTree
                        |> DictTree.walkBackwardFrom last (depth - 1)

                -- Convert dict into tree of "proto blocks" which is then "annotated" into the final model (a tree of "blocks").
                annotatedTree =
                    DictTree.buildForward depth start newTree [] Tree.tree
                        |> annotate nodes lastFinalizedBlock

                firstBlockHeight =
                    Tree.label annotatedTree |> .blockHeight

                gridSpec =
                    Maybe.withDefault (spec firstBlockHeight) model.gridSpec

                newDrawableChain =
                    Flatten.flattenTree ctx gridSpec lastFinalizedHeight 2 annotatedTree
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
                , gridSpec = Just gridSpec
            }

        _ ->
            model



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 TickSecond
        , if Transition.isComplete model.transition then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta (round >> OnAnimationFrame)
        ]



-- View


view : Context a -> Model -> Bool -> Element Msg
view ctx model showDevTools =
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

                gridSpec =
                    Maybe.withDefault (spec 0) model.gridSpec

                vcontext =
                    { gridSpec = gridSpec
                    , lastFinalized = lastFinalized
                    , nodes = nodes
                    , onBlockClick = Just BlockClicked
                    , selectedBlock = model.selectedBlock
                    }
            in
            column [ width fill, height fill, inFront (viewDebugButtons showDevTools) ]
                [ el
                    [ centerX
                    , centerY
                    , spacing (round gridSpec.gutterHeight)
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
                [ text "Dev tools:"
                , viewButton (Just SaveHistory) "Save History"
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
