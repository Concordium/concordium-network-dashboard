import { Elm } from "./elm/Main"
import { setup as clipboardSetup } from "./Clipboard"
import { setup as storageSetup } from "./Storage"

window.loadApp = (isProduction) => {
  const { clientWidth, clientHeight } = document.documentElement
  const app = Elm.Main.init({
    flags: {
      window: {
        width: clientWidth,
        height: clientHeight,
      },
      isProduction,
    }
  })

  storageSetup(app)
  clipboardSetup(app)
}
