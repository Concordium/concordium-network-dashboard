module Pages.ChainViz exposing (..)

import Chain
import Element exposing (..)
import TypesDashboard exposing (Msg(..))
import WidgetsDashboard exposing (..)


view model =
    map ChainMsg <| Chain.view model.chainModel
