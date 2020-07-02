module Config exposing (..)


type Mode
    = Local
    | Staging
    | Production


config : Mode
config =
    -- Tweak me when developing locally to test
    Staging


collector : String
collector =
    case config of
        Local ->
            "http://127.0.0.1:12000"

        Staging ->
            -- Once deployed the routing for both collector and middleware is through the same URL
            "https://dashboard.eu.staging.concordium.com"

        Production ->
            -- Once deployed the routing for both collector and middleware is through the same URL
            -- In production use path relative to current URL
            ""


middleware : String
middleware =
    case config of
        Local ->
            "http://localhost:8081"

        Staging ->
            -- Once deployed the routing for both collector and middleware is through the same URL
            "https://dashboard.eu.staging.concordium.com"

        Production ->
            -- Once deployed the routing for both collector and middleware is through the same URL
            -- In production use path relative to current URL
            ""
