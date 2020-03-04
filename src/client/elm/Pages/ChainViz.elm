module Pages.ChainViz exposing (..)

import Chain
import Element exposing (..)
import TypesDashboard exposing (Msg(..))


view model =
    el
        [ width fill
        , height fill
        ]
        (Chain.view model.chainModel)
        |> Element.map ChainMsg
