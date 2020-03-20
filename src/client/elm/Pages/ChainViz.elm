module Pages.ChainViz exposing (..)

import Chain
import Dashboard.Widgets exposing (content, remoteDataView, viewSummaryWidgets)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Explorer.View
import Types exposing (Model, Msg(..))


view : Model -> Element Msg
view model =
    content <|
        column [ width fill, spacing 20 ]
            [ viewSummaryWidgets model model.nodes
            , el
                [ width fill
                , height fill
                , Border.color <| model.palette.bg2
                , Border.rounded 4
                , Border.width 2
                ]
                (Chain.view model model.chainModel False)
                |> Element.map ChainMsg
            , Explorer.View.view model
            ]
