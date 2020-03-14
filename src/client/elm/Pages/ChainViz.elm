module Pages.ChainViz exposing (..)

import Chain
import ColorsDashboard exposing (..)
import Dashboard.Widgets exposing (content, remoteDataView, summaryWidgets)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import TypesDashboard exposing (Model, Msg(..))


view : Model -> Element Msg
view model =
    content <|
        column [ width fill, spacing 20 ]
            [ summaryWidgets model model.nodes
            , el
                [ width fill
                , Border.color <| moduleGrey
                , Border.rounded 4
                , Border.width 2
                ]
                (Chain.view model.chainModel)
                |> Element.map ChainMsg
            ]
