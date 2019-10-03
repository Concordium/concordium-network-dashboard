module Chain.Spec exposing (Spec, spec)


type alias Spec =
    { gutterWidth : Float
    , gutterHeight : Float
    , blockHeight : Float
    , blockWidth : Float
    , nodeIndicatorHeight : Float
    }


spec : Spec
spec =
    { gutterWidth = 30.0
    , gutterHeight = 24.0
    , blockHeight = 36.0
    , blockWidth = 64.0
    , nodeIndicatorHeight = 10.0
    }
