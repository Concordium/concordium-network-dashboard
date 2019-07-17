module View exposing (view)

import Browser
import Color
import Color.Interpolate exposing (Space(..), interpolate)
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Graph exposing (nodes)
import Html exposing (Html)
import Types exposing (..)
import ViewRewardGraph exposing (..)
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
                                ViewRewardGraph.view
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
        [ width (px 250)
        , height fill
        , Background.color <|
            toUI <|
                interpolate LAB (Color.rgb 1 1 1) nodeBackground 0.99
        ]
        (row [] [])
