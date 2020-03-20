module NodeHelpers exposing (..)

import Dict exposing (..)
import Dict.Extra as Dict
import Types exposing (..)


findNodeById : String -> Dict Host NetworkNode -> Maybe NetworkNode
findNodeById nodeId nodes =
    case Dict.find (\_ n -> n.nodeId == nodeId) nodes of
        Just ( _, node ) ->
            Just node

        Nothing ->
            Nothing


nodeTypeById : String -> Dict Host NetworkNode -> String
nodeTypeById nodeId nodes =
    nodes |> findNodeById nodeId |> Maybe.map .peerType |> Maybe.withDefault "Node"
