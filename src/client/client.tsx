import { Elm } from "./elm/Main"
import { setup as storageSetup } from "./Storage"
import { setup as clipboardSetup } from "./Clipboard"

document.addEventListener("DOMContentLoaded", function () {

  const layoutViewportSize = () => ({
    width: document.documentElement.clientWidth,
    height: document.documentElement.clientHeight
  })

  let app = Elm.Main.init({
    flags: layoutViewportSize()
  })

  storageSetup(app)
  clipboardSetup(app)
})

// Reload every 15 minutes or so - dumb way to keep our office screens on latest UI version
setTimeout(function() { window.location.reload() }, 15 * 60 * 1000);
