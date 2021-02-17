module Helpers exposing (..)

import Element exposing (..)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode


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
