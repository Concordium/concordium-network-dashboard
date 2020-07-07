import { Elm } from "./elm/Main"
import { setup as clipboardSetup } from "./Clipboard"
import { setup as storageSetup } from "./Storage"

document.addEventListener("DOMContentLoaded", function () {
  const layoutViewportSize = () => ({
    width: document.documentElement.clientWidth,
    height: document.documentElement.clientHeight
  })

  const app = Elm.Main.init({
    flags: layoutViewportSize()
  })

  storageSetup(app)
  clipboardSetup(app)
})
