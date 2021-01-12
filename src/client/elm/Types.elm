module Types exposing (..)

import BigInt exposing (BigInt)
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


{-| Amount of tokens; represented in GTU as microGTU using BigInt.
-}
type Amount
    = MicroGtu BigInt


amountToString : Amount -> String
amountToString (MicroGtu bigInt) =
    let
        padded =
            bigInt
                |> BigInt.toString
                |> String.padLeft (fracPartLength + 1) '0'

        amountString =
            String.dropRight fracPartLength padded ++ "." ++ String.right fracPartLength padded
    in
    amountString ++ " GTU"


{-| Decode amount in μGTU which is represented as a string.
-}
decodeAmount : D.Decoder Amount
decodeAmount =
    D.string
        |> D.andThen
            (\str ->
                case amountFromString str of
                    Just amount ->
                        D.succeed amount

                    Nothing ->
                        D.fail "Invalid amount"
            )


fracPartLength : Int
fracPartLength =
    6


amountFromString : String -> Maybe Amount
amountFromString str =
    BigInt.fromIntString str
        |> Maybe.map MicroGtu


amountFromInt : Int -> Amount
amountFromInt =
    BigInt.fromInt >> MicroGtu


scaleAmount : Float -> Amount -> Amount
scaleAmount s (MicroGtu bigInt) =
    let
        granularity =
            100000
    in
    MicroGtu <|
        BigInt.div (BigInt.mul bigInt <| BigInt.fromInt <| round <| s * granularity) (BigInt.fromInt granularity)


amountToInt : Amount -> Int
amountToInt (MicroGtu bigInt) =
    BigInt.toString bigInt
        |> String.toInt
        |> Maybe.withDefault 0


unsafeAmountRelation : Amount -> Amount -> Float
unsafeAmountRelation (MicroGtu numinator) (MicroGtu denominator) =
    BigInt.div numinator denominator
        |> BigInt.toString
        |> String.toInt
        |> Maybe.withDefault 0
        |> toFloat
        |> (\n -> n / 1000000)


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


addAmounts : Amount -> Amount -> Amount
addAmounts (MicroGtu left) (MicroGtu right) =
    MicroGtu <| BigInt.add left right


subAmounts : Amount -> Amount -> Amount
subAmounts (MicroGtu left) (MicroGtu right) =
    MicroGtu <| BigInt.sub left right


zeroAmount : Amount
zeroAmount =
    amountFromInt 0


sumAmounts : List Amount -> Amount
sumAmounts amounts =
    List.foldl addAmounts zeroAmount amounts


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
