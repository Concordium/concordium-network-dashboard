module TimeHelpers exposing (..)

import Time
import Time.Extra exposing (toOffset)


formatTime : Time.Zone -> Time.Posix -> String
formatTime zone posix =
    (String.fromInt <| Time.toYear zone posix)
        ++ "-"
        ++ (String.padLeft 2 '0' <| String.fromInt <| monthToInt <| Time.toMonth zone posix)
        ++ "-"
        ++ (String.padLeft 2 '0' <| String.fromInt <| Time.toDay zone posix)
        ++ " "
        ++ (String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone posix)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone posix)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt <| Time.toSecond zone posix)
        ++ "."
        ++ (String.padLeft 2 '0' <| String.left 2 <| String.fromInt <| Time.toMillis zone posix)
        -- @TODO use the timezone lookup in future
        ++ formatTimezone zone posix


formatTimezone : Time.Zone -> Time.Posix -> String
formatTimezone zone posix =
    let
        offset =
            toOffset zone posix // 60

        sign =
            if offset < 0 then
                "-"

            else
                "+"
    in
    " UTC" ++ sign ++ String.fromInt offset


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
