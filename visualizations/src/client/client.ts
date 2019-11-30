import { Elm } from "./elm/Main"
import io from 'socket.io-client'


document.addEventListener("DOMContentLoaded", function () {

  const layoutViewportSize = () => ({
    width: document.documentElement.clientWidth,
    height: document.documentElement.clientHeight
  })

  let app = Elm.Main.init({
    flags: layoutViewportSize()
  })

})
