module CollectionHelpers exposing (..)

import Dict
import Dict.Extra as Dict
import List.Extra as List


{-| Return the element which occurs the most times in the input list.
If there are multiple elements that occurs the same number of times, a random one is chosen. -}
maxFrequency : List comparable -> Maybe comparable
maxFrequency list =
    list
        |> Dict.frequencies
        |> Dict.toList
        |> List.maximumBy Tuple.second
        |> Maybe.map Tuple.first


{-| Return the median element of the input list, which is assumed to be sorted.
If there are an even number of elements, the "leftmost middle" element is returned. -}
median : List a -> Maybe a
median sortedList =
    List.getAt (List.length sortedList // 2) sortedList
