module Pages.Network exposing (..)

import Context exposing (Context)
import Dict
import Element exposing (..)
import Network exposing (Model, Msg)
import Network.NodesTable exposing (..)
import Network.Widgets exposing (..)


view : Context a -> Model -> Element Msg
view ctx model =
    content <|
        column [ spacing 30, width fill ]
            [ viewSummaryWidgets ctx model.nodes
            , remoteDataView ctx.palette
                (\nodes ->
                    let
                        listNodes =
                            nodes
                                |> Dict.toList
                                |> List.map Tuple.second

                        sortedNodes =
                            sortNodesMode model.sortMode listNodes
                    in
                    nodesTable ctx model.sortMode sortedNodes
                )
                model.nodes
            ]
