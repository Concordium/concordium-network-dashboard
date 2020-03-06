module Pages.ChainViz exposing (..)

import Chain
import ColorsDashboard exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import TypesDashboard exposing (Model, Msg(..))
import View.SummaryWidgets exposing (summaryWidgets)
import WidgetsDashboard exposing (content)


view : Model -> Element Msg
view model =
    content <|
        column [ width fill, spacing 20 ]
            [ summaryWidgets model
            , el
                [ width fill
                , Border.color <| moduleGrey
                , Border.rounded 4
                , Border.width 2
                ]
                (Chain.view model.chainModel)
                |> Element.map ChainMsg
            ]
