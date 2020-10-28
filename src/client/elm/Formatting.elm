module Formatting exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Palette exposing (Palette)
import Round
import Time exposing (Posix)
import Time.Distance exposing (inWordsWithConfig)
import Time.Distance.Types exposing (..)
import Time.Extra


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


asSecondsAgo : Time.Posix -> Maybe Time.Posix -> String
asSecondsAgo currentTime maybeTargetTime =
    case maybeTargetTime of
        Just targetTime ->
            -- Handle case where app is initialized and we don't yet have a real currentTime value.
            -- TODO (mbo) Make time a Maybe if this is a real concern.
            if Time.posixToMillis currentTime == 0 then
                ""

            else
                Time.Extra.diff Time.Extra.Second Time.utc targetTime currentTime
                    |> max 0
                    -- Clamp time to zero to avoid displaying time from the future.
                    |> secondsAsText

        Nothing ->
            "N/A"


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


ellipsis : Int -> String -> String
ellipsis maxLength string =
    if String.length string > maxLength then
        String.left maxLength string ++ "..."

    else
        string
