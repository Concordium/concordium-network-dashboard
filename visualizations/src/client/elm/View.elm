module View exposing (view)

import Browser
import Color exposing (toRgba)
import Color.Interpolate exposing (Space(..), interpolate)
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Graph exposing (Edge, Graph, Node, NodeContext, nodes)
import Html exposing (Html)
import IntDict
import Ionicon.Ios
import RewardGraph exposing (..)
import RewardGraph.Static exposing (..)
import SvgWidgets exposing (..)
import Types exposing (..)
import Widgets exposing (..)


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
                        [ viewAside model
                        , el [ width fill, height fill ]
                            (html <|
                                RewardGraph.Static.view
                                    model.window
                                    model.selectedNode
                                    model.graph
                            )
                        ]
                    ]
        ]
    }


viewAside : Model -> Element Msg
viewAside model =
    el
        [ width (px 320)
        , height fill
        , Background.color <|
            toUI <|
                interpolate LAB (Color.rgb 1 1 1) nodeBackground 0.99
        ]
        (column [ width fill, height fill, paddingXY 40 60, spacing 40 ]
            [ row
                [ Font.size 16
                , Font.color <| toUI blue
                , Font.bold
                , spacing 10
                ]
                [ image [ width (px 25), height (px 25) ]
                    { src = "/assets/concordium.svg"
                    , description = "Concordium Logo"
                    }
                , text "Rewardgraph Explorer"
                ]
            , el [] (maybeViewConnectionList model.selectedNode model.graph)
            ]
        )


maybeViewConnectionList : Maybe Int -> Graph NodeSpec EdgeSpec -> Element Msg
maybeViewConnectionList maybeSelected graph =
    let
        maybeNode =
            maybeSelected
                |> Maybe.andThen (\selected -> Graph.get selected graph)
    in
    Maybe.map2
        (\selected node -> viewConnectionList selected node graph)
        maybeSelected
        maybeNode
        |> Maybe.withDefault
            (el
                [ Font.size 14
                , Font.color <| toUI midGray
                ]
                (text "Select a node to edit.")
            )


viewConnectionList : Int -> NodeContext NodeSpec EdgeSpec -> Graph NodeSpec EdgeSpec -> Element Msg
viewConnectionList selectedId origin graph =
    let
        outgoingEdges =
            List.filter (\edge -> edge.from == selectedId) (Graph.edges graph)

        originLabel =
            origin.node.label.label
                |> List.foldr (\a b -> a ++ " " ++ b) ""

        originValue =
            origin.node.label.value
                |> round

        originColor =
            nodeColorFromId selectedId graph

        outgoing =
            origin.outgoing
    in
    column [ Font.size 14, spacing 28 ]
        (el
            [ Font.size 16
            , Font.color (toUI originColor)
            , Border.solid
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , paddingXY 0 8
            ]
            (text (originLabel ++ " (" ++ String.fromInt originValue ++ ")"))
            :: List.map
                (\edge -> maybeViewConnection origin.node.label edge graph)
                outgoingEdges
        )


maybeViewConnection : NodeSpec -> Edge EdgeSpec -> Graph NodeSpec EdgeSpec -> Element Msg
maybeViewConnection origin edge graph =
    let
        maybeTarget =
            Graph.get edge.to graph
                |> Maybe.map (.node >> .label)
    in
    Maybe.map (\target -> viewConnection origin target edge graph) maybeTarget
        |> Maybe.withDefault none


viewConnection : NodeSpec -> NodeSpec -> Edge EdgeSpec -> Graph NodeSpec EdgeSpec -> Element Msg
viewConnection origin target edge graph =
    let
        targetColor =
            nodeColor target

        targetLabel =
            List.foldr (\a b -> a ++ " " ++ b) "" target.label

        originColor =
            nodeColor origin
    in
    column []
        [ row [ spacing 10 ]
            [ el [ moveLeft 5 ] (html <| Ionicon.Ios.arrowThinRight 30 (toRgba originColor))
            , column [ spacing 8 ]
                [ el [ Font.color (toUI targetColor) ] (text targetLabel)
                , row [ Font.color (toUI midGray), spacing 16 ]
                    [ row [ spacing 8 ]
                        [ SvgWidgets.clock 17 0.6
                        , Input.text
                            [ width (px 60)
                            , height (px 24)
                            , Background.color <| rgba 1 1 1 0.02
                            , Border.width 1
                            , Border.color <| rgba 1 1 1 0.05
                            , Border.rounded 6
                            ]
                            { onChange = EdgeIntervalChanged edge.from edge.to
                            , text = edge.label.intervalString
                            , placeholder = Nothing
                            , label = Input.labelHidden "Interval"
                            }
                        ]
                    , row [ spacing 8 ]
                        [ amountIcon 18 18 midGray
                        , Input.text
                            [ width (px 60)
                            , height (px 24)
                            , Background.color <| rgba 1 1 1 0.02
                            , Border.width 1
                            , Border.color <| rgba 1 1 1 0.05
                            , Border.rounded 6
                            ]
                            { onChange = EdgeValueChanged edge.from edge.to
                            , text = edge.label.valueString
                            , placeholder = Nothing
                            , label = Input.labelHidden "Value"
                            }
                        ]
                    ]
                ]
            ]
        ]
