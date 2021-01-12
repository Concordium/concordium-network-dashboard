module Types exposing (..)

import Dict
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, resolve)
import Maybe
import Time


{-| Parallel to Concordium.Types.Address.
Must stay in sync.
-}
type Address
    = AddressAccount AccountAddress
    | AddressContract ContractAddress


addressDecoder : D.Decoder Address
addressDecoder =
    let
        decode : String -> D.Decoder Address
        decode tipe =
            case tipe of
                "AddressAccount" ->
                    D.map AddressAccount <| D.field "address" accountAddressDecoder

                "AddressContract" ->
                    D.map AddressContract <| D.field "address" contractAddressDecoder

                _ ->
                    D.fail "Invalid address. Expected type AddressAccount or AddressContract."
    in
    D.succeed decode
        |> required "type" D.string
        |> resolve


addressToString : Address -> String
addressToString addr =
    case addr of
        AddressAccount accAddr ->
            accAddr

        AddressContract contrAddr ->
            contractAddressToString contrAddr


{-| Parallel to Concordium.Types.Address.
Must stay in sync.
-}
type alias AccountAddress =
    String


accountAddressDecoder : D.Decoder AccountAddress
accountAddressDecoder =
    D.string


{-| Parallel to Concordium.Types.ContractAddress.
Must stay in sync.
-}
type alias ContractAddress =
    { index : Int
    , subindex : Int
    }


contractAddressDecoder : D.Decoder ContractAddress
contractAddressDecoder =
    D.succeed ContractAddress
        |> required "index" D.int
        |> required "subindex" D.int


{-| Show contractAddress in the following format: "<INDEX,SUBINDEX>".
-}
contractAddressToString : ContractAddress -> String
contractAddressToString { index, subindex } =
    "<" ++ String.fromInt index ++ "," ++ String.fromInt subindex ++ ">"


{-| Parallel to Concordium.Types.Transactions.AccountAmounts.
Must stay in sync.
-}
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


{-| Parallel to Concordium.Types.Timestamp.
Must stay in sync.

It wraps an Int representing _seconds_ since unix epoch.

-}
type alias Timestamp =
    Time.Posix


timestampDecoder : D.Decoder Timestamp
timestampDecoder =
    D.map Time.millisToPosix <| D.int


type alias ReleaseSchedule =
    List ( Timestamp, Amount )


releaseScheduleDecoder : D.Decoder ReleaseSchedule
releaseScheduleDecoder =
    let
        pairDecoder =
            D.map2 Tuple.pair
                (D.index 0 timestampDecoder)
                (D.index 1 decodeAmount)
    in
    D.list pairDecoder


{-| Parallel to Concordium.Wasm.ContractEvent.
Must stay in sync.
-}
type alias ContractEvent =
    String


contractEventDecoder : D.Decoder ContractEvent
contractEventDecoder =
    D.string


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
                "approx. " ++ value

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


roundTo : Int -> Float -> Float
roundTo decimals n =
    let
        p =
            10 ^ toFloat decimals
    in
    (n * p |> round |> toFloat) / p


floorTo : Int -> Float -> Float
floorTo decimals n =
    let
        p =
            10 ^ toFloat decimals
    in
    (n * p |> floor |> toFloat) / p


amountFromFloat : Float -> Amount
amountFromFloat f =
    Gtu { value = String.fromFloat <| roundTo 6 f, hasRoundingError = True }


amountToFloat : Amount -> Maybe Float
amountToFloat (Gtu amount) =
    String.toFloat amount.value


addAmounts : Amount -> Amount -> Maybe Amount
addAmounts left right =
    Maybe.map2 (\l r -> amountFromFloat <| l + r) (amountToFloat left) (amountToFloat right)


zeroAmount : Amount
zeroAmount =
    Gtu { value = "0.0", hasRoundingError = False }


unsafeAmountToFloat : Amount -> Float
unsafeAmountToFloat amount =
    Maybe.withDefault 0 <| amountToFloat amount


unsafeAddAmounts : Amount -> Amount -> Amount
unsafeAddAmounts left right =
    Maybe.withDefault zeroAmount (addAmounts left right)


unsafeSumAmounts : List Amount -> Amount
unsafeSumAmounts amounts =
    List.foldl unsafeAddAmounts zeroAmount amounts


type alias ModuleRef =
    String


{-| The init function name on-chain is the contract name prefixed with "init\_",
so we strip this during decoding.
-}
type alias InitName =
    String


contractInitNameDecoder : D.Decoder InitName
contractInitNameDecoder =
    D.string
        |> D.andThen
            (\str ->
                if String.startsWith "init_" str then
                    D.succeed <| String.dropLeft 5 str

                else
                    D.fail "Invalid init function name"
            )


{-| The receive function name on-chain is the contract name appended with "."
and then a name for the specific function.
We split these during decoding.
-}
type alias ReceiveName =
    { contractName : String, functionName : String }


contractReceiveNameDecoder : D.Decoder ReceiveName
contractReceiveNameDecoder =
    D.string
        |> D.andThen
            (\str ->
                let
                    parts =
                        String.split "." str
                in
                case ( List.head parts, List.tail parts ) of
                    ( Just contractName, Just functionNameParts ) ->
                        D.succeed <| { contractName = contractName, functionName = String.join "." functionNameParts }

                    _ ->
                        D.fail "Invalid receive function name"
            )


type alias BakerId =
    Int


type alias BakerAggregationVerifyKey =
    String
