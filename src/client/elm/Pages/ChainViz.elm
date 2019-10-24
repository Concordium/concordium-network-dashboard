module Pages.ChainViz exposing (..)

import Chain
import Element exposing (..)
import WidgetsDashboard exposing (..)


view model =
    [ row
        [ width fill
        , height fill
        ]
        [ Chain.viewFlattenedChain model.chainModel.flatTree ]
    ]
