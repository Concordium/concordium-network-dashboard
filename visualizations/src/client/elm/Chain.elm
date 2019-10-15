module Chain exposing (Model, Msg(..), init, update, view)

import Chain.Api as Api exposing (Block, BlockStatus(..), Node, annotateChain)
import Chain.Connector exposing (..)
import Chain.Spec exposing (..)
import Color exposing (..)
import Color.Interpolate exposing (..)
import Colors exposing (..)
import Deque exposing (Deque)
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
    { nodes : Deque (List Node)
    , chainTree : List (Tree Block)
    , errors : List Http.Error
    }



-- Put comments around this when checking out the mocked Nodes


init : ( Model, Cmd Msg )
init =
    ( { nodes = Deque.fromList []
      , chainTree = []
      , errors = []
      }
    , Api.getNodeInfo GotNodeInfo
    )



{--| Mocked init for testing

init : ( Model, Cmd Msg )
init =
     { nodes = Deque.fromList [ mockNodes ]
     , chainTree = []
     , errors = []
     }
        |> update (GotNodeInfo (Success mockNodes))

}

mockNodes =
    [ [ "a", "b", "c", "d" ]
    , [ "a", "b", "c", "d" ]
    , [ "a", "b", "q", "w", "e" ]
    , [ "a", "b", "q", "w" ]
    , [ "a", "t", "z", "u" ]
    , [ "a", "t", "z", "i" ]
    ]
        |> List.map
            (\l ->
                let
                    final =
                        Maybe.withDefault "..." (List.head l)

                    ancestors =
                        List.drop 1 l |> List.reverse
                in
                { nodeName = ""
                , nodeId = ""
                , bestBlock = Maybe.withDefault "..." (List.last l)
                , bestBlockHeight = 0
                , finalizedBlock = final
                , finalizedBlockHeight = 0
                , ancestorsSinceBestBlock = ancestors
                }
            )

--}
--
--
-- Update


type Msg
    = GotNodeInfo (WebData (List Node))
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotNodeInfo (Success nodeInfo) ->
            ( { model
                | nodes = updateNodes nodeInfo model.nodes
                , chainTree =
                    Api.buildChain (List.map Api.prepareBlockSequence nodeInfo)
                        |> List.map (annotateChain nodeInfo)
              }
            , Cmd.none
            )

        GotNodeInfo (Failure error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        GotNodeInfo _ ->
            ( model, Cmd.none )

        Tick time ->
            ( model, Api.getNodeInfo GotNodeInfo )


updateNodes : List Node -> Deque (List Node) -> Deque (List Node)
updateNodes new current =
    Deque.pushFront new current
        |> Deque.dropRight (max 0 (Deque.length current - 3))



-- View


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
    Keyed.el [ alignTop, animateFadeIn ]
        ( block.hash
        , row []
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
                    ]
                    (el [ centerX, centerY ] (text block.hash))
                ]
            , connector spec (fromUI <| blockBackground block.status) block.connectors
            ]
        )


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
