module CollectionHelpers exposing (..)

import Dict
import Dict.Extra as Dict
import List.Extra as List


maxFrequency : List comparable -> Maybe comparable
maxFrequency list =
    list
        |> Dict.frequencies
        |> Dict.toList
        |> List.maximumBy Tuple.second
        |> Maybe.map Tuple.first
