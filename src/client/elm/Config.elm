module Config exposing (Config, Environment, defaultConfig, isProduction, parseEnv)


type alias Config =
    { environment : Environment
    , collectorUrl : String
    , middlewareUrl : String
    }


type Environment
    = Development DevelopmentTarget
    | Production


type DevelopmentTarget
    = Local
    | Staging
    | Testnet


devTarget : DevelopmentTarget
devTarget =
    -- Tweak me when developing locally to test
    Local


isProduction : Config -> Bool
isProduction cfg =
    cfg.environment == Production


parseEnv : Bool -> Environment
parseEnv isProd =
    if isProd then
        Production

    else
        Development devTarget


defaultConfig : Environment -> Config
defaultConfig env =
    { environment = env
    , collectorUrl = "http://54.154.159.60:8080"
    , middlewareUrl = "http://54.154.159.60:8081"
    }
