port module Analytics exposing (..)

{-| Ports related to Google Analytics
-}


{-| Use to inform analytics of which page is currently shown
-}
port setPageConfig : String -> Cmd msg


{-| Use to set whether the user have granted permission to use cookies
-}
port setCookieConsent : Bool -> Cmd msg
