module Chain exposing
    ( Model
    , Msg(..)
    , init
    , rebuild
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
import Chain.Grid as Grid exposing (GridSpec)
import Chain.Interpolate as Interpolate
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
import Material.Icons.Sharp as MaterialIcons
import Material.Icons.Types exposing (Coloring(..))
import Maybe
import Palette
import RemoteData exposing (..)
import Route exposing (Route)
import Set
import Task
import Time exposing (..)
import Tooltip exposing (stringTooltipAboveWidget, stringTooltipAlignedRight)
import Transition exposing (..)
import Tree exposing (Tree)
import Network exposing (filterNodesByHeight)
import Api
import Platform.Cmd as Cmd



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
    , gridSpec : Maybe GridSpec
    , maxWidth : Int
    , selectedBlock : Maybe String
    , minFinalizedHeight : Maybe Int
    }


init : Int -> ( Model, Cmd Msg )
init maxWidth =
    ( { nodes = []
      , initialBlockHeight = Nothing
      , lastFinalized = Nothing
      , bestBlock = Nothing
      , tree = DictTree.init
      , annotatedTree = Nothing
      , transition = Transition.constant emptyDrawableChain
      , errors = []
      , replay = Nothing
      , gridSpec = Nothing
      , maxWidth = maxWidth
      , selectedBlock = Nothing
      , minFinalizedHeight = Nothing
      }
    , Api.getConsensusStatus ReceivedConsensusStatus
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
    | GotParentBlockByHeight ProtoBlock (WebData String)
    | LoadedHistory File
    | ReplayHistory
    | SaveHistory
    | GotHistoryString String
    | TickSecond Posix
    | OnAnimationFrame Int
    | BlockClicked String
    | MaxWidthChanged Int
    | ReceivedConsensusStatus (Api.ApiResult Api.ConsensusStatus)
    | ReceivedBlockResponse (Api.ApiResult Api.BlockResponse)


update : Context a -> Msg -> Model -> (Model, Cmd Msg)
update ctx msg model =
    case msg of
        GotNodeInfo (Success nodeInfo) ->
            if Just nodeInfo /= List.head model.nodes then
                updateChain ctx (filterNodesByHeight model.minFinalizedHeight nodeInfo) model

            else
                ( model, Cmd.none )

        GotNodeInfo (Failure error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        GotNodeInfo _ ->
            ( model, Cmd.none )

        GotParentBlockByHeight child (Success parentHash) ->
            if model.replay == Nothing then
                { model
                    | tree =
                        DictTree.addAll
                            [ [ ( Tuple.first child - 1, parentHash ), child ] ]
                            model.tree
                }
                    |> rebuild ctx

            else
                ( model, Cmd.none )

        GotParentBlockByHeight _ (Failure error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        GotParentBlockByHeight _ _ ->
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
                    let
                        ( replayModel, _ ) =
                            init model.maxWidth
                    in
                    ( { replayModel
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
            case model.minFinalizedHeight of
                Nothing -> (model, Cmd.none)
                Just _ ->
                  case model.replay of
                      Nothing ->
                          ( model
                          , Build.getNodeInfo GotNodeInfo
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
            ( model, Nav.pushUrl ctx.key (Route.toString <| Route.Chain (Just hash)) )

        MaxWidthChanged maxWidth ->
            let
                newModel =
                    { model | maxWidth = maxWidth }
            in
            case List.head model.nodes of
                Nothing ->
                    ( newModel, Cmd.none )

                Just nodes ->
                    updateChain ctx (filterNodesByHeight model.minFinalizedHeight nodes) newModel
        ReceivedConsensusStatus res ->
            case res of
                Ok consensusStatus ->
                    ( model
                    , Api.getBlockInfo consensusStatus.currentEraGenesisBlock ReceivedBlockResponse
                    )

                Err err ->
                    ( model, Cmd.none )
        ReceivedBlockResponse blockInfoRes ->
            case blockInfoRes of
                Ok (Api.Block blockInfo) ->
                    ( { model
                        | minFinalizedHeight = Just blockInfo.blockHeight
                      }
                    , Build.getNodeInfo GotNodeInfo
                    )

                Ok (Api.BlockNotFound _) ->
                    ( model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

selectBlock : Model -> String -> Model
selectBlock model hash =
    { model | selectedBlock = Just hash }


selectedBlockFinalizationChanged : Model -> Model -> Bool
selectedBlockFinalizationChanged lastModel currentModel =
    if lastModel.selectedBlock == currentModel.selectedBlock then
        Maybe.map3
            (\selectedBlockHeight lastFinalizedHeight lastModelLastFinalizedHeight ->
                let
                    wasNotFinalized =
                        selectedBlockHeight > lastModelLastFinalizedHeight

                    newIsFinalized =
                        lastModelLastFinalizedHeight /= lastFinalizedHeight

                    selectedIsFinalized =
                        selectedBlockHeight <= lastFinalizedHeight
                in
                wasNotFinalized && newIsFinalized && selectedIsFinalized
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


updateChain : Context a -> List Node -> Model -> ( Model, Cmd Msg )
updateChain ctx nodes model =
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
        ( Just bestBlock, Just finalBlock ) ->
            let
                lastFinalizedHeight =
                    Tuple.first finalBlock

                -- Set the block offset in the grid specification, if it does not exist yet.
                -- this is used as a starting point for shifting the viewBox, to avoid
                -- a bug that occurs if the viewBox position is too far out.
                gridSpec =
                    Maybe.withDefault (spec lastFinalizedHeight) model.gridSpec

                targetDepth =
                    Grid.maxCells model.maxWidth gridSpec

                -- Find "highest" blocks of all branches up to max height of `targetDepth` from the last finalized block.
                -- The reason for not going beyond that is to ensure that this block remains included in the view.
                ( stepsWalkedForwards, last ) =
                    newTree
                        |> DictTree.walkForwardFrom finalBlock targetDepth
                        |> List.maximumBy Tuple.first
                        |> Maybe.withDefault ( 0, bestBlock )

                -- Walk back from the last block to find the start block, which is two blocks before the last finalized block.
                ( stepsWalkedBackwards, start ) =
                    newTree
                        |> DictTree.walkBackwardFrom last (targetDepth - 2)

                -- Convert dict into tree of "proto blocks" which is then "annotated" into the final model (a tree of "blocks").
                annotatedTree =
                    DictTree.buildForward targetDepth start newTree [] Tree.tree
                        |> annotate nodes lastFinalizedHeight

                ( firstBlockHash, firstBlockHeight ) =
                    let
                        first =
                            Tree.label annotatedTree
                    in
                    ( first.hash, first.blockHeight )

                newDrawableChain =
                    Flatten.flattenTree ctx gridSpec lastFinalizedHeight 2 annotatedTree

                loadAdditionalBlockCmd =
                    if newDrawableChain.width < targetDepth then
                        Build.getBlockByHeight
                            (firstBlockHeight - 1)
                            (GotParentBlockByHeight ( firstBlockHeight, firstBlockHash ))

                    else
                        Cmd.none
            in
            ( { model
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
            , loadAdditionalBlockCmd
            )

        _ ->
            ( model, Cmd.none )


rebuild : Context a -> Model -> ( Model, Cmd Msg )
rebuild ctx model =
    case List.head model.nodes of
        Just nodes ->
            updateChain ctx (filterNodesByHeight model.minFinalizedHeight nodes) model

        Nothing ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 2000 TickSecond
        , if Transition.isComplete model.transition then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta (round >> OnAnimationFrame)
        ]



-- View


view : Theme a -> Model -> Bool -> Element Msg
view theme model showDevTools =
    case (model.lastFinalized, model.minFinalizedHeight) of
        (Just lastFinalized, Just _) ->
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
                    , maxWidth = model.maxWidth
                    }
            in
            column
                [ width fill
                , height fill
                , inFront (viewDebugButtons showDevTools)
                , inFront (viewInfoIcon theme "The purple bars represent the proportion of nodes which have the given block as best block.")
                ]
                [ el
                    [ centerX
                    , centerY
                    , spacing (round gridSpec.gutterHeight)
                    , clip
                    ]
                    (html <| View.viewChain theme vcontext currentDrawableChain)
                ]

        _ ->
            none


viewInfoIcon : Theme a -> String -> Element msg
viewInfoIcon ctx description =
    el [ padding 17, alignRight ]
        (el
            [ padding 3
            , Font.color (ctx.palette.c3 |> Palette.withAlphaEl 0.3)
            , stringTooltipAlignedRight ctx (text description)
            ]
            (html <| MaterialIcons.info 16 Inherit)
        )


viewDebugButtons : Bool -> Element Msg
viewDebugButtons show =
    case show of
        True ->
            row
                [ padding 10
                , spacing 10
                , Background.color <| Element.rgba 0 0 0 0.1
                , Border.rounded 5
                ]
                [ text "Dev tools:"
                , viewButton (Just SaveHistory) "Save Replay"
                , viewButton (Just ReplayHistory) "Load Replay"
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
