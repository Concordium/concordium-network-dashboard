module Config exposing (..)


type Mode
    = Staging
    | Local


config =
    Staging


collector =
    case config of
        Local ->
            "http://127.0.0.1:12000"

        Staging ->
            -- Once deployed the routing for both collector and middleware is through the same URL
            "https://dashboard.eu.staging.concordium.com"


middleware =
    -- "/"
    case config of
        Local ->
            "http://localhost:8081"

        Staging ->
            -- Once deployed the routing for both collector and middleware is through the same URL
            "https://dashboard.eu.staging.concordium.com"
