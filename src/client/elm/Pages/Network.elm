module Pages.Network exposing (..)

import Network.NodesTable exposing (..)
import Network.Widgets exposing (..)
import Dict
import Element exposing (..)
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
                    nodesTable model model.sortMode sortedNodes
                )
                model.nodes
            ]
