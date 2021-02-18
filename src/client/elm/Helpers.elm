module Helpers exposing (..)

import Element exposing (..)
import Html exposing (Html, a)
import Html.Events
import Json.Decode as Decode
import Set exposing (Set)


{-| Emit msg when the enter key is released after being pressed
-}
onEnter : msg -> Element.Attribute msg
onEnter msg =
    onKeyup "Enter" msg


{-| Emit msg when a key is released after being pressed, takes the key as a string
-}
onKeyup : String -> msg -> Element.Attribute msg
onKeyup keyup msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == keyup then
                            Decode.succeed msg

                        else
                            Decode.fail <| "Not the '" ++ keyup ++ "' key"
                    )
            )
        )


{-| Creating a constant function
-}
const : a -> b -> a
const x _ =
    x


{-| If key is member of set, it is removed, otherwise it is inserted
-}
toggleSetMember : comparable -> Set comparable -> Set comparable
toggleSetMember key set =
    if Set.member key set then
        Set.remove key set

    else
        Set.insert key set
