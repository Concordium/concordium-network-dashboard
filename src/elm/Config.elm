module Config exposing (Config, Environment, cookiePrivacyUrl, defaultConfig, isProduction, parseEnv)


type alias Config =
    { environment : Environment
    }


type Environment
    = Development
    | Production


isProduction : Config -> Bool
isProduction cfg =
    cfg.environment == Production


parseEnv : Bool -> Environment
parseEnv isProd =
    if isProd then
        Production

    else
        Development


defaultConfig : Environment -> Config
defaultConfig env =
    { environment = env
    }


{-| Pointing to an external privacy policy used by the cookie consent banner
-}
cookiePrivacyUrl : String
cookiePrivacyUrl =
    "https://concordium.com/privacy-policy/"
