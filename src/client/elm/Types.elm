module Types exposing (..)

import Dict
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, resolve)


type alias AccountAddress =
    String


accountAddressDecoder : D.Decoder AccountAddress
accountAddressDecoder =
    D.string


type alias ContractAddress =
    { index : Int
    , subindex : Int
    }


contractAddressDecoder : D.Decoder ContractAddress
contractAddressDecoder =
    D.succeed ContractAddress
        |> required "index" D.int
        |> required "subindex" D.int


type AccountInfo
    = AddressAccount String
    | AddressContract String
    | AddressUnknown


accountInfoAddress : AccountInfo -> String
accountInfoAddress accountInfo =
    case accountInfo of
        AddressAccount address ->
            address

        AddressContract address ->
            address

        AddressUnknown ->
            ""


accountInfoDecoder : D.Decoder AccountInfo
accountInfoDecoder =
    D.succeed
        (\tipe address ->
            case tipe of
                "AddressAccount" ->
                    AddressAccount address

                "AddressContract" ->
                    AddressContract address

                _ ->
                    AddressUnknown
        )
        |> required "type" D.string
        |> required "address" D.string


type alias AccountAmounts =
    Dict.Dict AccountAddress Amount


accountAmountsDecoder : D.Decoder AccountAmounts
accountAmountsDecoder =
    let
        accAmntPairDecoder =
            D.succeed toPair
                |> required "address" accountAddressDecoder
                |> required "amount" decodeAmount
                |> resolve

        toPair : AccountAddress -> Amount -> D.Decoder ( AccountAddress, Amount )
        toPair accAddr amnt =
            D.succeed ( accAddr, amnt )
    in
    D.map Dict.fromList <| D.list accAmntPairDecoder


{-| Energy is represented as an Int.
-}
type Energy
    = Energy Int


decodeEnergy : D.Decoder Energy
decodeEnergy =
    D.int |> D.map Energy


{-| Amount of tokens; represented in GTU as a "floating point" string.
Representing the number as a string avoids rounding errors.
The field 'hasRoundingError' is true if the amount was decoded from an int larger than the largest "safe value"
(2^53-1; see <https://package.elm-lang.org/packages/elm/core/latest/Basics#Int>).
-}
type Amount
    = Gtu
        { value : String
        , hasRoundingError : Bool
        }


amountToString : Amount -> String
amountToString amount =
    (case amount of
        Gtu { value, hasRoundingError } ->
            if hasRoundingError then
                "~" ++ value

            else
                value
    )
        ++ " GTU"


{-| Decode amount in μGTU from an integer which is optionally represented as a string.
The integer format is legacy and will be phased out (TODO CB-271).
-}
decodeAmount : D.Decoder Amount
decodeAmount =
    D.oneOf
        [ -- String format: allows us to do custom parsing.
          D.string |> D.map amountFromString

        -- Legacy format: passing amount as int. Might have rounding errors.
        , D.int |> D.map amountFromInt
        ]


fracPartLength : Int
fracPartLength =
    6


amountFromString : String -> Amount
amountFromString str =
    let
        left =
            str
                |> String.dropRight fracPartLength

        right =
            str
                |> String.dropLeft (String.length left)
                |> String.padLeft fracPartLength '0'
    in
    Gtu
        { value =
            if String.isEmpty left then
                "0." ++ right

            else
                left ++ "." ++ right
        , hasRoundingError = False
        }


amountFromInt : Int -> Amount
amountFromInt value =
    case amountFromString (String.fromInt value) of
        Gtu v ->
            Gtu { v | hasRoundingError = value >= 2 ^ 53 }
