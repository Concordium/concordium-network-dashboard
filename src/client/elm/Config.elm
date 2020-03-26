module Config exposing (..)


type Mode
    = Staging
    | Local


config =
    Staging


summariesUrl =
    -- "http://127.0.0.1:12000/nodesSummary"
    case config of
        Local ->
            "/nodesSummary"

        Staging ->
            "https://dashboard.eu.staging.concordium.com/nodesSummary"


middleware =
    case config of
        Local ->
            "http://localhost:8081"

        Staging ->
            "https://dashboard.eu.staging.concordium.com/"
