module Pages.Home exposing (..)

import Dashboard.Formatting exposing (..)
import Dashboard.NodesTable exposing (..)
import Dashboard.Widgets exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import List.Extra as List
import Types exposing (..)


view : Model -> Element Msg
view model =
    content <|
        column [ spacing 30, width fill ]
            [ viewSummaryWidgets model model.nodes
            , remoteDataView model.palette
                (\nodes ->
                    let
                        listNodes =
                            nodes
                                |> Dict.toList
                                |> List.map Tuple.second

                        sortedNodes =
                            sortNodesMode model.sortMode listNodes
                    in
                    content <| nodesTable model model.sortMode sortedNodes
                )
                model.nodes
            ]
