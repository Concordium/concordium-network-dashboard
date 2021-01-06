module Transaction.SummaryTest exposing (..)

import Expect exposing (Expectation)
import Json.Decode as D exposing (Error(..))
import Test exposing (..)
import Types as T


suite : Test
suite =
    describe "The Transaction.Summary module"
        [ test "Cannot decode from empty" <|
            \_ -> Expect.err (D.decodeString T.decodeAmount "")
        , describe "Decode from int"
            [ test "Can decode '0'" <|
                \_ -> Expect.equal (Ok <| T.Gtu { value = "0.000000", hasRoundingError = False }) (D.decodeString T.decodeAmount "0")
            , test "Cannot decode '00' (invalid JSON)" <|
                \_ -> Expect.err (D.decodeString T.decodeAmount "00")
            , test "Can decode '1'" <|
                \_ -> Expect.equal (Ok <| T.Gtu { value = "0.000001", hasRoundingError = False }) (D.decodeString T.decodeAmount "1")
            , test "Can decode small number (has only fractional part)" <|
                \_ -> Expect.equal (Ok <| T.Gtu { value = "0.123456", hasRoundingError = False }) (D.decodeString T.decodeAmount "123456")
            , test "Can decode midsize number (has only integer part)" <|
                \_ -> Expect.equal (Ok <| T.Gtu { value = "1.000000", hasRoundingError = False }) (D.decodeString T.decodeAmount "1000000")
            , test "Can decode large number (has both integer and fractional part)" <|
                \_ -> Expect.equal (Ok <| T.Gtu { value = "1.234567", hasRoundingError = False }) (D.decodeString T.decodeAmount "1234567")
            , test "Can decode very large number (max value without rounding error: 2^53-1)" <|
                \_ -> Expect.equal (Ok <| T.Gtu { value = "9007199254.740991", hasRoundingError = False }) (D.decodeString T.decodeAmount "9007199254740991")
            , test "Can decode too large number (min invalid with potential rounding error: 2^53)" <|
                \_ ->
                    Expect.true
                        "expected successful result with rounding error"
                        (case D.decodeString T.decodeAmount "9117199254740992" of
                            Ok (T.Gtu { hasRoundingError }) ->
                                hasRoundingError

                            _ ->
                                False
                        )
            ]
        , describe "Decode from string" <|
            [ test "Can decode enormous number" <|
                \_ -> Expect.equal (Ok <| T.Gtu { value = "1234567890987.654321", hasRoundingError = False }) (D.decodeString T.decodeAmount "\"1234567890987654321\"")
            ]
        ]
