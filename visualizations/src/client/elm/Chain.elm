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
import Element.Keyed as Keyed
import Http
import List.Extra as List
import RemoteData exposing (..)
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
      }
    , Api.getNodeInfo GotNodeInfo
    )



-- Update


type Msg
    = GotNodeInfo (WebData (List Node))
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNodeInfo (Success nodeInfo) ->
            ( updateChain 5 nodeInfo model
            , Cmd.none
            )

        GotNodeInfo (Failure error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        GotNodeInfo _ ->
            ( model, Cmd.none )

        Tick time ->
            ( model, Api.getNodeInfo GotNodeInfo )


updateNodes : List Node -> List (List Node) -> List (List Node)
updateNodes new current =
    List.append [ new ] current
        |> List.take 3


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
        ( Just lastFinalized, Just bestBlock ) ->
            let
                ( walkedForward, last ) =
                    ntree
                        |> DTree.walkForwardFrom bestBlock depth
                        |> List.maximumBy Tuple.first
                        |> Maybe.withDefault ( 0, bestBlock )

                ( walkedBackward, start ) =
                    ntree
                        |> DTree.walkBackwardFrom last depth

                annotatedTree =
                    DTree.buildForward depth start ntree [] Tree.tree
                        |> annotate [] bestBlock
            in
            { model
                | annotatedTree = Just annotatedTree
                , nodes = updateNodes nodes model.nodes
                , tree = ntree
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


view : Tree Block -> Element msg
view tree =
    column [ centerX, centerY, spacing (round spec.gutterHeight) ]
        [ Tree.restructure
            viewBlock
            viewChain
            tree
        ]


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
