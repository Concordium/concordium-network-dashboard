port module Storage exposing (..)

-- Ported from Web Wallet

import Dict
import Json.Decode as D
import Json.Decode.Pipeline exposing (..)
import Json.Encode as E


port saveDoc : E.Value -> Cmd msg


port fetchDoc : String -> Cmd msg


port receiveDoc : (D.Value -> msg) -> Sub msg


port loadAll : () -> Cmd msg


port clear : () -> Cmd msg


type alias StorageDoc =
    { id : String
    , tipe : String -- @TODO make it a concrete type
    , value : E.Value
    }


encodeStorageDoc item =
    E.object
        [ ( "id", E.string item.id )
        , ( "type", E.string item.tipe )
        , ( "value", item.value )
        ]


decodeStorageDoc =
    D.succeed StorageDoc
        |> required "id" D.string
        |> required "type" D.string
        |> required "value" D.value


save : StorageDoc -> Cmd msg
save doc =
    saveDoc <| encodeStorageDoc doc
