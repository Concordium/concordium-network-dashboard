module Widgets exposing (asSecondsAgo, asTimeAgoDuration, averageStatSecondsFor, formatPing, header, majorityStatFor, withinHighestStatFor)

import Colors exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Iso8601
import List.Extra as List
import Round
import Time
import Time.Distance exposing (inWordsWithConfig)
import Time.Distance.I18n as I18n
import Time.Extra
import Types exposing (..)


header =
    row [ width fill ]
        [ link [] { url = "/", label = image [ height (px 20) ] { src = "/assets/images/concordium-logo.png", description = "Concordium Logo" } }
        , row [ alignRight, spacing 20 ]
            [ link [] { url = "/", label = text "Dashboard" }
            , link [] { url = "/nodegraph", label = text "Graph" }
            ]
        ]


asTimeAgoDuration duration =
    let
        point =
            -- Arbitrary point, doesn't matter we care about figuring out the duration
            1552573958904

        timePoint =
            Time.millisToPosix point

        offsetTimePoint =
            Time.millisToPosix (point - round duration)
    in
    inWordsWithConfig { withAffix = False } I18n.en offsetTimePoint timePoint


asSecondsAgo currentTime targetTime =
    case Iso8601.toTime targetTime of
        Err x ->
            "-"

        Ok p ->
            if Time.Extra.diff Time.Extra.Second Time.utc currentTime (Time.millisToPosix 0) == 0 then
                -- Handle case where app is initialised and we don't yet have a real currentTime value
                "-"

            else
                let
                    secondsAgo =
                        Time.Extra.diff Time.Extra.Second Time.utc p currentTime

                    seconds =
                        remainderBy 60 secondsAgo

                    minutes =
                        remainderBy 60 ((secondsAgo - seconds) // 60)

                    hours =
                        (secondsAgo - seconds - (minutes * 60)) // 3600

                    parts =
                        [ String.fromInt seconds
                        , String.fromInt minutes
                        , String.fromInt hours
                        ]
                            |> List.filter (\a -> a /= "0")

                    partsString =
                        List.map2 Tuple.pair parts [ "s", "m", "h" ]
                            |> List.map (\( a, b ) -> a ++ b)
                            |> List.reverse
                in
                String.concat partsString


majorityStatFor getter default nodes =
    -- For the given node attribute, finds majority value across all nodes and returns that, or the default if unknown
    let
        stats =
            nodes
                |> Dict.toList
                |> List.map Tuple.second
                |> List.map getter

        highestResult =
            stats
                |> List.foldl
                    (\v dict ->
                        Dict.update v
                            (\mCount ->
                                case mCount of
                                    Just count ->
                                        Just <| count + 1

                                    Nothing ->
                                        Just 1
                            )
                            dict
                    )
                    Dict.empty
                |> Dict.toList
                |> List.maximumBy (\( attr, count ) -> count)
    in
    case highestResult of
        Just ( highestSeenKey, groupedDictCount ) ->
            highestSeenKey

        Nothing ->
            default


withinHighestStatFor : (NetworkNode -> Float) -> b -> Dict.Dict String NetworkNode -> (NetworkNode -> Maybe String) -> Maybe String
withinHighestStatFor getter default nodes withinGetter =
    -- For the given node attribute, finds highest value across all nodes and returns that, or the default if unknown
    nodes
        |> Dict.toList
        |> List.map Tuple.second
        |> List.maximumBy (\node -> getter node)
        |> Maybe.map withinGetter
        |> Maybe.withDefault (Just "")


averageStatSecondsFor getter nodes =
    let
        dataPoints =
            nodes
                |> Dict.toList
                |> List.map Tuple.second
                |> List.map getter
                |> List.filterMap (\a -> a)

        isNaNInt x =
            x /= x

        result =
            List.sum dataPoints / toFloat (List.length dataPoints)
    in
    if Dict.size nodes == 0 || isNaNInt result then
        "-"

    else
        Round.round 2 result ++ "s"


formatPing averagePing =
    case averagePing of
        Just ping ->
            if ping < 1000 then
                text <| Round.round 2 ping ++ "ms"

            else if ping < 1000 * 60 then
                el [ Font.color orange ] (text <| Round.round 2 (ping / 1000) ++ "s")

            else
                el [ Font.color red ] (text <| "> 60s")

        Nothing ->
            el [ Font.color red ] (text "n/a")
