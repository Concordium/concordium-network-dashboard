module Dashboard.Formatting exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Iso8601
import List.Extra as List
import Palette exposing (Palette)
import Round
import Time exposing (Posix)
import Time.Distance exposing (inWordsWithConfig)
import Time.Distance.Types exposing (..)
import Time.Extra
import Types exposing (..)


formatTimeBetween : Posix -> Posix -> String
formatTimeBetween start end =
    let
        interval =
            Time.posixToMillis end - Time.posixToMillis start
    in
    formatTimeIntervalMs interval


formatTimeIntervalMs : Int -> String
formatTimeIntervalMs milliseconds =
    let
        wholeDivisionAndRemainder a b =
            ( a // b, remainderBy b a )

        ( tS, ms ) =
            wholeDivisionAndRemainder milliseconds 1000

        ( tM, s ) =
            wholeDivisionAndRemainder tS 60

        ( tH, m ) =
            wholeDivisionAndRemainder tM 60

        ( tD, h ) =
            wholeDivisionAndRemainder tH 24

        formatIfNotZero n unit =
            if n > 0 then
                String.fromInt n ++ unit

            else
                ""
    in
    formatIfNotZero tD "d"
        ++ formatIfNotZero h "h"
        ++ formatIfNotZero m "m"
        ++ formatIfNotZero s "s"



--++ formatIfNotZero ms "ms"


asTimeAgoDuration : Float -> String
asTimeAgoDuration duration =
    let
        offset =
            Time.millisToPosix (round duration)
    in
    inWordsWithConfig { withAffix = False } enCompact offset (Time.millisToPosix 0)


{-| A more compact version of I18n.en
-}
enCompact : Locale
enCompact { withAffix } tense distanceId =
    let
        toStr =
            String.fromInt

        maybeAffix str =
            case ( withAffix, tense ) of
                ( True, Past ) ->
                    str ++ " ago"

                ( True, Future ) ->
                    "in " ++ str

                ( False, _ ) ->
                    str
    in
    (case distanceId of
        LessThanXSeconds i ->
            if i == 1 then
                "1s"

            else
                toStr i ++ "s"

        HalfAMinute ->
            "30s"

        LessThanXMinutes i ->
            if i == 1 then
                "< 1m"

            else
                "< " ++ toStr i ++ "m"

        XMinutes i ->
            if i == 1 then
                "1m"

            else
                toStr i ++ "m"

        AboutXHours i ->
            if i == 1 then
                "1h"

            else
                toStr i ++ "h"

        XDays i ->
            if i == 1 then
                "1d"

            else
                toStr i ++ "d"

        AboutXMonths i ->
            if i == 1 then
                "1mon"

            else
                toStr i ++ "mon"

        XMonths i ->
            if i == 1 then
                "1mon"

            else
                toStr i ++ "mon"

        AboutXYears i ->
            if i == 1 then
                "1y"

            else
                toStr i ++ "y"

        OverXYears i ->
            if i == 1 then
                "> 1y"

            else
                "> " ++ toStr i ++ "y"

        AlmostXYears i ->
            if i == 1 then
                "1y"

            else
                toStr i ++ "y"
    )
        |> maybeAffix


asSecondsAgo : Time.Posix -> String -> String
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
                in
                secondsAsText secondsAgo


secondsAsText : Int -> String
secondsAsText secondsAgo =
    let
        seconds =
            remainderBy 60 secondsAgo

        minutes =
            remainderBy 60 ((secondsAgo - seconds) // 60)

        hours =
            (secondsAgo - seconds - (minutes * 60)) // 3600

        parts =
            [ String.fromInt seconds ++ "s"
            , if hours == 0 && minutes == 0 then
                ""

              else
                String.fromInt minutes
            , if hours == 0 then
                ""

              else
                String.fromInt hours
            ]
                |> List.filter (\a -> a /= "")

        partsString =
            List.map2 Tuple.pair parts [ "", "m", "h" ]
                |> List.map (\( a, b ) -> a ++ b)
                |> List.reverse
    in
    String.concat partsString


{-| For the given node attribute, finds majority value across all nodes
and returns that, or the default if unknown.
-}
majorityStatFor :
    (NetworkNode -> comparable)
    -> comparable
    -> Dict Host NetworkNode
    -> comparable
majorityStatFor getter default nodes =
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


{-| For the given node attribute, finds highest value across all
nodes and returns that, or the default if unknown
-}
withinHighestStatFor :
    (NetworkNode -> Float)
    -> Dict.Dict String NetworkNode
    -> (NetworkNode -> Maybe String)
    -> Maybe String
withinHighestStatFor getter nodes withinGetter =
    nodes
        |> Dict.toList
        |> List.map Tuple.second
        |> List.maximumBy (\node -> getter node)
        |> Maybe.map withinGetter
        |> Maybe.withDefault (Just "")


averageStatSecondsFor : (b -> Maybe Float) -> Dict a b -> String
averageStatSecondsFor getter nodes =
    let
        dataPoints =
            nodes
                |> Dict.toList
                |> List.map Tuple.second
                |> List.filterMap getter

        isNaNInt x =
            x /= x

        result =
            List.sum dataPoints / toFloat (List.length dataPoints)
    in
    if Dict.size nodes == 0 || isNaNInt result then
        "-"

    else
        Round.round 2 result ++ "s"


formatPing : Palette Color -> Maybe Float -> Element msg
formatPing palette averagePing =
    case averagePing of
        Just ping ->
            if ping < 1000 then
                text <| Round.round 2 ping ++ "ms"

            else if ping < 1000 * 60 then
                el [ Font.alignRight, Font.color palette.danger ] (text <| Round.round 2 (ping / 1000) ++ "s")

            else
                el [ Font.alignRight, Font.color palette.failure ] (text <| "> 60s")

        Nothing ->
            el [ Font.color palette.failure ] (text "n/a")


justs : List (Maybe a) -> List a
justs =
    List.foldl
        (\v acc ->
            case v of
                Just x ->
                    [ x ] ++ acc

                Nothing ->
                    acc
        )
        []


ellipsis : Int -> String -> String
ellipsis maxLength string =
    if String.length string > maxLength then
        String.left 30 string ++ "..."

    else
        string
