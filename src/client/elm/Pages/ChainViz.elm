module Pages.ChainViz exposing (..)

import Chain
import Element exposing (..)
import TypesDashboard exposing (Msg(..))
import WidgetsDashboard exposing (..)


view model =
    [ row
        [ width fill
        , height fill
        ]
        [ map ChainMsg <| Chain.view model.chainModel ]
    ]
