module View exposing (view)

import Browser
import Dict
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
import WebsiteColors exposing (..)
import Widgets exposing (..)


view : Model -> Browser.Document Msg
view model =
    { title = "Concordium Visualizations"
    , body =
        [ theme <|
            case model.currentPage of
                Home ->
                    [ el [ width fill, height fill, centerX, centerY ]
                        (html <|
                            ViewRewardGraph.viewNodes
                                model.selectedNode
                                (nodes model.graph)
                        )
                    ]
        ]
    }
