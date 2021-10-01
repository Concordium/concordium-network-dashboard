module ScrollHelpers exposing (..)

import Browser.Dom
import Task


scrollPageToTop : a -> Cmd a
scrollPageToTop msg =
    Task.perform (\_ -> msg) (Browser.Dom.setViewport 0 0)
