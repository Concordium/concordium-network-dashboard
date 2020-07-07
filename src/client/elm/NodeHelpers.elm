module NodeHelpers exposing (..)

import Dict exposing (..)
import Dict.Extra as Dict
import Types exposing (..)


findNodeById : String -> Dict Host NetworkNode -> Maybe NetworkNode
findNodeById nodeId =
     Dict.find (\_ n -> n.nodeId == nodeId) >> Maybe.map Tuple.second
